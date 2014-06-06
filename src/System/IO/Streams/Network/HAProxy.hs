{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE Trustworthy              #-}
{-# LANGUAGE TupleSections            #-}

module System.IO.Streams.Network.HAProxy
  ( behindHAProxy
  , behindHAProxyWithAddresses
  , ProxyInfo
  , makeProxyInfo
  , getSourceAddr
  , getDestAddr
  ) where


------------------------------------------------------------------------------
import           Control.Applicative                        ((<$>), (<|>))
import           Control.Monad                              (void, when)
import           Data.Attoparsec.ByteString                 (anyWord8)
import           Data.Attoparsec.ByteString.Char8           (Parser, char, decimal, skipWhile, string, take, takeWhile1)
import           Data.Bits                                  (unsafeShiftR, (.&.))
import qualified Data.ByteString                            as S8
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Unsafe                     as S
import           Data.Typeable                              (Typeable)
import           Data.Word                                  (Word16, Word32, Word8)
import           Foreign.C.Types                            (CUShort (..))
import           Foreign.Ptr                                (castPtr)
import           Foreign.Storable                           (peek)
import qualified Network.Socket                             as N
import           Prelude                                    hiding (take)
import           System.IO.Streams                          (InputStream, OutputStream)
import qualified System.IO.Streams                          as Streams
import qualified System.IO.Streams.Attoparsec               as Streams
import           System.IO.Streams.Network.Internal.Address (getSockAddr)
import           System.IO.Unsafe                           (unsafePerformIO)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Parses the proxy headers emitted by HAProxy and runs a user action with
-- the origin/destination socket addresses provided by HAProxy. Will throw a
-- 'Sockets.ParseException' if the protocol header cannot be parsed properly.
--
behindHAProxy :: N.Socket         -- ^ A socket you've just accepted
              -> (ProxyInfo
                  -> InputStream ByteString
                  -> OutputStream ByteString
                  -> IO a)
              -> IO a
behindHAProxy socket m = do
    sockets      <- Streams.socketToStreams socket
    origSrcAddr  <- N.getPeerName socket
    origDestAddr <- N.getSocketName socket
    behindHAProxyWithAddresses (origSrcAddr, origDestAddr) sockets m


------------------------------------------------------------------------------
-- | Like 'behindHAProxy', but allows the socket addresses and input/output
-- streams to be passed in instead of created based on an input 'Socket'.
-- Useful for unit tests.
--
behindHAProxyWithAddresses
  :: (N.SockAddr, N.SockAddr)     -- ^ source and destination addresses
  -> (InputStream ByteString, OutputStream ByteString)  -- ^ socket streams
  -> (ProxyInfo
          -> InputStream ByteString
          -> OutputStream ByteString
          -> IO a)              -- ^ user function
  -> IO a
behindHAProxyWithAddresses (origSrcAddr, origDestAddr) (is0, os) m = do
    -- 536 bytes as per spec
    is <- Streams.throwIfProducesMoreThan 536 is0
    (!isOld, !mbOldInfo) <- Streams.parseFromStream
                              (((True,) <$> parseOldHaProxy)
                               <|> return (False, Nothing)) is
    proxyInfo <-
     if isOld
       then maybe (return $! makeProxyInfo origSrcAddr origDestAddr)
                  (\(srcAddr, srcPort, destAddr, destPort) -> do
                      (_, s) <- getSockAddr srcPort srcAddr
                      (_, d) <- getSockAddr destPort destAddr
                      return $! makeProxyInfo s d)
                  mbOldInfo
       else do
           (s, d) <- Streams.parseFromStream
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
getDestAddr p = _destAddr p


------------------------------------------------------------------------------
makeProxyInfo :: N.SockAddr -> N.SockAddr -> ProxyInfo
makeProxyInfo srcAddr destAddr = ProxyInfo srcAddr destAddr


------------------------------------------------------------------------------
parseFamily :: Parser Bool
parseFamily = (string "TCP4" >> return True)
                <|> (string "TCP6" >> return True)
                <|> (string "UNKNOWN" >> return False)


------------------------------------------------------------------------------
parseOldHaProxy :: Parser (Maybe (ByteString, Int, ByteString, Int))
parseOldHaProxy = do
    string "PROXY "
    gotFamily <- parseFamily
    case gotFamily of
      False -> skipWhile (/= '\r') >> string "\r\n" >> return Nothing
      True  -> do
          char ' '
          srcAddress <- takeWhile1 (/= ' ')
          char ' '
          destAddress <- takeWhile1 (/= ' ')
          char ' '
          srcPort <- decimal
          char ' '
          destPort <- decimal
          string "\r\n"
          return $! Just $! (srcAddress, srcPort, destAddress, destPort)


------------------------------------------------------------------------------
protocolHeader :: ByteString
protocolHeader = S8.pack [ 0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D
                         , 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A ]
{-# NOINLINE protocolHeader #-}


------------------------------------------------------------------------------
parseNewHaProxy :: N.SockAddr -> N.SockAddr -> Parser (N.SockAddr, N.SockAddr)
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

    addressLen <- ntohs <$> snarf16

    case () of
        !_ | command == 0x0 || family == 0x0 || protocol == 0x0   -- LOCAL
                -> handleLocal addressLen
           | family == 0x1 -> handleIPv4 addressLen
           | family == 0x2 -> handleIPv6 addressLen
           | family == 0x3 -> handleUnix addressLen
           | otherwise     -> fail $ "Bad family " ++ show family

  where
    toProtocol 1 = Just N.Stream
    toProtocol 2 = Just N.Datagram
    toProtocol _ = Nothing

    handleLocal addressLen = do
        -- skip N bytes and return the original addresses
        when (addressLen > 500) $ fail $ "suspiciously long address "
                                          ++ show addressLen
        void $ take (fromIntegral addressLen)
        return $! ( origSrcAddr, origDestAddr )

    handleIPv4 addressLen = do
        when (addressLen < 12) $ fail $ "bad address length "
                                         ++ show addressLen
                                         ++ " for IPv4"
        let nskip = addressLen - 12
        srcAddr  <- snarf32
        destAddr <- snarf32
        srcPort  <- snarf16
        destPort <- snarf16
        void $ take $ fromIntegral nskip

        -- Note: we actually want the brain-dead constructors here
        return $! ( N.SockAddrInet (N.PortNum srcPort) srcAddr
                  , N.SockAddrInet (N.PortNum destPort) destAddr
                  )

    handleIPv6 addressLen = do
        let scopeId = 0xe   -- means "global", kludge alert!
        let flow    = 0

        when (addressLen < 36) $ fail $ "bad address length "
                                         ++ show addressLen
                                         ++ " for IPv6"
        let nskip = addressLen - 36
        s1 <- snarf32
        s2 <- snarf32
        s3 <- snarf32
        s4 <- snarf32

        d1 <- snarf32
        d2 <- snarf32
        d3 <- snarf32
        d4 <- snarf32

        sp <- snarf16
        dp <- snarf16

        void $ take $ fromIntegral nskip

        return $! ( N.SockAddrInet6 (N.PortNum sp) flow (s1, s2, s3, s4) scopeId
                  , N.SockAddrInet6 (N.PortNum dp) flow (d1, d2, d3, d4) scopeId
                  )

    handleUnix addressLen = do
        undefined


foreign import ccall unsafe "iostreams_ntohs" c_ntohs :: CUShort -> CUShort

ntohs :: Word16 -> Word16
ntohs = fromIntegral . c_ntohs . fromIntegral


snarf32 :: Parser Word32
snarf32 = do
    s <- take 4
    return $! unsafePerformIO $! S.unsafeUseAsCString s $ peek . castPtr


snarf16 :: Parser Word16
snarf16 = do
    s <- take 2
    return $! unsafePerformIO $! S.unsafeUseAsCString s $ peek . castPtr
