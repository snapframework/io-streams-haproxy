{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module System.IO.Streams.Network.HAProxy
  ( behindHAProxy
  , ProxyInfo
  , HaProxyException
  , makeProxyInfo
  , getSourceAddr
  , getDestAddr
  ) where

------------------------------------------------------------------------------
import           Control.Applicative                        ((<$>), (<|>))
import           Control.Exception                          (Exception)
import           Control.Monad                              (void, when)
import           Data.Attoparsec.ByteString                 (anyWord8)
import           Data.Attoparsec.ByteString.Char8           (IResult (..), Parser, Result, char, decimal, skipWhile, string, take, takeWhile1)
import           Data.Bits                                  (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString                            as S8
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Char8                      as S
import           Data.Typeable                              (Typeable)
import           Data.Word                                  (Word16, Word32, Word8)
import qualified Network.Socket                             as N
import qualified Network.Socket.ByteString                  as NB
import           Prelude                                    hiding (take)
import           System.IO.Streams                          (InputStream, OutputStream)
import qualified System.IO.Streams                          as Streams
import qualified System.IO.Streams.Attoparsec               as Streams
import           System.IO.Streams.Network.Internal.Address (getSockAddr)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data HaProxyException = HaProxyException { _haProxyExceptionReason :: ByteString }
  deriving (Show, Typeable)

instance Exception HaProxyException


------------------------------------------------------------------------------
behindHAProxy :: N.Socket         -- ^ A socket you've just accepted
              -> (ProxyInfo
                  -> InputStream ByteString
                  -> OutputStream ByteString
                  -> IO a)
              -> IO a
behindHAProxy socket m = do
    (is, os) <- Streams.socketToStreams socket
    (isOld, mbOldInfo) <- Streams.parseFromStream
                            (((True,) <$> parseOldHaProxy)
                             <|> return (False, Nothing)) is
    origSrcAddr  <- N.getPeerName socket
    origDestAddr <- N.getSocketName socket
    proxyInfo <-
     if isOld
       then maybe (return $! makeProxyInfo origSrcAddr origDestAddr)
                  (\(family, srcAddr, srcPort, destAddr, destPort) -> do
                      (_, s) <- getSockAddr srcPort srcAddr
                      (_, d) <- getSockAddr destPort destAddr
                      return $! makeProxyInfo s d)
                  mbOldInfo
       else do
           (_, s, d) <- Streams.parseFromStream
                          (parseNewHaProxy origSrcAddr origDestAddr) is
           return $! makeProxyInfo s d
    m proxyInfo is os


------------------------------------------------------------------------------
data ProxyInfo = ProxyInfo {
      _sourceAddr :: N.SockAddr
    , _destAddr   :: N.SockAddr
    } deriving (Typeable, Show)


------------------------------------------------------------------------------
getSourceAddr :: ProxyInfo -> N.SockAddr
getSourceAddr p = _sourceAddr p


------------------------------------------------------------------------------
getDestAddr :: ProxyInfo -> N.SockAddr
getDestAddr p = _sourceAddr p


------------------------------------------------------------------------------
makeProxyInfo :: N.SockAddr -> N.SockAddr -> ProxyInfo
makeProxyInfo srcAddr destAddr = ProxyInfo srcAddr destAddr


------------------------------------------------------------------------------
parseFamily :: Parser (Maybe N.Family)
parseFamily = (string "TCP4" >> return (Just N.AF_INET))
                <|> (string "TCP6" >> return (Just N.AF_INET6))
                <|> (string "UNKNOWN" >> return Nothing)


------------------------------------------------------------------------------
parseOldHaProxy :: Parser (Maybe (N.Family, ByteString, Int, ByteString, Int))
parseOldHaProxy = do
    string "PROXY "
    mbFamily <- parseFamily
    case mbFamily of
      Nothing -> skipWhile (/= '\r') >> string "\r\n" >> return Nothing
      (Just family) -> do
          char ' '
          srcAddress <- takeWhile1 (not . (== ' '))
          char ' '
          destAddress <- takeWhile1 (not . (== ' '))
          char ' '
          srcPort <- decimal
          char ' '
          destPort <- decimal
          string "\r\n"
          return $! Just $! (family, srcAddress, srcPort, destAddress, destPort)


------------------------------------------------------------------------------
protocolHeader :: ByteString
protocolHeader = S8.pack [ 0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D
                         , 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A ]
{-# NOINLINE protocolHeader #-}


------------------------------------------------------------------------------
parseNewHaProxy :: N.SockAddr -> N.SockAddr -> Parser (N.Family, N.SockAddr, N.SockAddr)
parseNewHaProxy origSrcAddr origDestAddr = do
    string protocolHeader

    versionAndCommand <- anyWord8
    let version = (versionAndCommand .&. 0xF0) `unsafeShiftR` 4
    let command = (versionAndCommand .&. 0xF) :: Word8
    when (version /= 0x2) $ fail $ "Invalid protocol version: " ++ show version
    when (command > 1) $ fail $ "Invalid command: " ++ show command

    protocolAndFamily <- anyWord8
    let family = (protocolAndFamily .&. 0xF0) `unsafeShiftR` 4
    let protocol = (protocolAndFamily .&. 0xF) :: Word8

    -- VALUES FOR FAMILY
    -- 0x0 : AF_UNSPEC : the connection is forwarded for an unknown,
    -- unspecified or unsupported protocol. The sender should use this family
    -- when sending LOCAL commands or when dealing with unsupported protocol
    -- families. The receiver is free to accept the connection anyway and use
    -- the real endpoint addresses or to reject it. The receiver should ignore
    -- address information.

    -- 0x1 : AF_INET : the forwarded connection uses the AF_INET address family
    -- (IPv4). The addresses are exactly 4 bytes each in network byte order,
    -- followed by transport protocol information (typically ports).

    -- 0x2 : AF_INET6 : the forwarded connection uses the AF_INET6 address
    -- family (IPv6). The addresses are exactly 16 bytes each in network byte
    -- order, followed by transport protocol information (typically ports).
    --
    -- 0x3 : AF_UNIX : the forwarded connection uses the AF_UNIX address family
    -- (UNIX). The addresses are exactly 108 bytes each.
    let mbProtocol = toProtocol protocol

    addressLen <- n16

    (srcAddr, destAddr) <-
      case () of
          !_ | command == 0x0 || family == 0x0 || protocol == 0x0   -- LOCAL
                  -> handleLocal addressLen
             | family == 0x1 -> handleIPv4 addressLen
             | family == 0x2 -> handleIPv6 addressLen
             | family == 0x3 -> handleUnix addressLen
             | otherwise     -> fail $ "Bad family " ++ show family
    undefined

  where
    toProtocol 1 = Just N.Stream
    toProtocol 2 = Just N.Datagram
    toProtocol _ = Nothing

    w8 :: Parser Word32
    w8 = fromIntegral <$> anyWord8

    le32 = do
        -- unfortunately this is the only reliable way of doing this are you on
        -- a big endian machine? too bad! (for now)
        b4 <- w8
        b3 <- w8
        b2 <- w8
        b1 <- w8
        return $! (b1 `unsafeShiftL` 24  .|.
                   b2 `unsafeShiftL` 16  .|.
                   b3 `unsafeShiftL` 8   .|.
                   b4)

    le16 :: Parser Word16
    le16 = do
        b2 <- w8
        b1 <- w8
        return $! fromIntegral $! (b1 `unsafeShiftL` 8  .|. b2)

    n16 = do
        b1 <- w8
        b2 <- w8
        return $! (b1 `unsafeShiftL` 8  .|. b2)

    handleLocal addressLen = do
        -- skip N bytes and return the original addresses
        when (addressLen > 500) $ fail $ "suspiciously long address "
                                          ++ show addressLen
        void $ take (fromIntegral addressLen)
        return $! ( origSrcAddr, origDestAddr )

    handleIPv4 addressLen = do
        when (addressLen /= 12) $ fail $ "bad address length "
                                         ++ show addressLen
                                         ++ " for IPv4"
        srcAddr  <- le32
        destAddr <- le32
        srcPort  <- le16
        destPort <- le16

        -- Note: we actually want the brain-dead constructors here
        return $! ( N.SockAddrInet (N.PortNum srcPort) srcAddr
                  , N.SockAddrInet (N.PortNum destPort) destAddr
                  )

    handleIPv6 addressLen = do
        let scopeId = 0xe   -- means "global", kludge alert!
        let flow    = 0

        when (addressLen /= 36) $ fail $ "bad address length "
                                         ++ show addressLen
                                         ++ " for IPv6"
        s1 <- le32
        s2 <- le32
        s3 <- le32
        s4 <- le32

        d1 <- le32
        d2 <- le32
        d3 <- le32
        d4 <- le32

        sp <- le16
        dp <- le16

        return $! ( N.SockAddrInet6 (N.PortNum sp) flow (s1, s2, s3, s4) scopeId
                  , N.SockAddrInet6 (N.PortNum dp) flow (d1, d2, d3, d4) scopeId
                  )

    handleUnix addressLen = do
        undefined
