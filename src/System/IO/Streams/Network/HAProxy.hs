{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE Trustworthy              #-}
{-# LANGUAGE TupleSections            #-}

{-|

HAProxy proxying protocol support (see
<http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt>) for applications
using io-streams. The proxy protocol allows information about a networked peer
(like remote address and port) to be propagated through a forwarding proxy that
is configured to speak this protocol.

This approach is safer than other alternatives like injecting a special HTTP
header (like "X-Forwarded-For") because the data is sent out of band, requests
without the proxy header fail, and proxy data cannot be spoofed by the client.

-}

module System.IO.Streams.Network.HAProxy
  (
  -- * Proxying requests.
    behindHAProxy
  , behindHAProxyWithAddresses
  -- * Information about proxied requests.
  , ProxyInfo
  , makeProxyInfo
  , getSourceAddr
  , getDestAddr
  , getFamily
  , getSocketType
  ) where

------------------------------------------------------------------------------
import           Control.Applicative                        ((<$>), (<|>))
import           Control.Monad                              (void, when)
import           Data.Attoparsec.ByteString                 (anyWord8)
import           Data.Attoparsec.ByteString.Char8           (Parser, char, decimal, skipWhile, string, take, takeWhile1)
import           Data.Bits                                  (unsafeShiftR, (.&.))
import qualified Data.ByteString                            as S8
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Char8                      as S
import qualified Data.ByteString.Unsafe                     as S
import           Data.Word                                  (Word16, Word32, Word8)
import           Foreign.C.Types                            (CUInt (..), CUShort (..))
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
-- We support version 1.5 of the protocol (both the "old" text protocol and the
-- "new" binary protocol.). Typed data fields after the addresses are not (yet)
-- supported.
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
    let (N.MkSocket _ _ !origSockType _ _) = socket
    behindHAProxyWithAddresses (origSrcAddr, origDestAddr, origSockType) sockets m


------------------------------------------------------------------------------
-- | Like 'behindHAProxy', but allows the socket addresses and input/output
-- streams to be passed in instead of created based on an input 'Socket'.
-- Useful for unit tests.
--
behindHAProxyWithAddresses
  :: (N.SockAddr, N.SockAddr, N.SocketType)     -- ^ source and destination addresses
  -> (InputStream ByteString, OutputStream ByteString)  -- ^ socket streams
  -> (ProxyInfo
          -> InputStream ByteString
          -> OutputStream ByteString
          -> IO a)              -- ^ user function
  -> IO a
behindHAProxyWithAddresses (origSrcAddr, origDestAddr, origSockType) (is0, os) m = do
    -- 536 bytes as per spec
    let origFamily = addrFamily origSrcAddr
    is <- Streams.throwIfProducesMoreThan 536 is0
    (!isOld, !mbOldInfo) <- Streams.parseFromStream
                              (((True,) <$> parseOldHaProxy)
                               <|> return (False, Nothing)) is
    proxyInfo <-
     if isOld
       then maybe (return $! makeProxyInfo origSrcAddr origDestAddr origFamily
                                           origSockType)
                  (\(srcAddr, srcPort, destAddr, destPort, f) -> do
                      (_, s) <- getSockAddr srcPort srcAddr
                      (_, d) <- getSockAddr destPort destAddr
                      return $! makeProxyInfo s d f origSockType)
                  mbOldInfo
       else do
           (s, d, f, st) <- Streams.parseFromStream
                              (parseNewHaProxy origSrcAddr origDestAddr origSockType) is
           return $! makeProxyInfo s d f st
    m proxyInfo is os


------------------------------------------------------------------------------
-- | Stores information about the proxied request.
data ProxyInfo = ProxyInfo {
      _sourceAddr :: N.SockAddr
    , _destAddr   :: N.SockAddr
    , _family     :: N.Family
    , _sockType   :: N.SocketType
    } deriving (Show)


------------------------------------------------------------------------------
-- | Gets the 'N.Family' of the proxied request (i.e. IPv4/IPv6/Unix domain
-- sockets).
getFamily :: ProxyInfo -> N.Family
getFamily p = _family p


------------------------------------------------------------------------------
-- | Gets the 'N.SocketType' of the proxied request (UDP/TCP).
getSocketType :: ProxyInfo -> N.SocketType
getSocketType p = _sockType p


------------------------------------------------------------------------------
-- | Gets the network address of the source node for this request (i.e. the
-- client).
getSourceAddr :: ProxyInfo -> N.SockAddr
getSourceAddr p = _sourceAddr p


------------------------------------------------------------------------------
-- | Gets the network address of the destination node for this request (i.e. the
-- client).
getDestAddr :: ProxyInfo -> N.SockAddr
getDestAddr p = _destAddr p


------------------------------------------------------------------------------
-- | Makes a 'ProxyInfo' object.
makeProxyInfo :: N.SockAddr      -- ^ the source address
              -> N.SockAddr      -- ^ the destination address
              -> N.Family        -- ^ the socket family
              -> N.SocketType    -- ^ the socket type
              -> ProxyInfo
makeProxyInfo srcAddr destAddr f st = ProxyInfo srcAddr destAddr f st


------------------------------------------------------------------------------
parseFamily :: Parser (Maybe N.Family)
parseFamily = (string "TCP4" >> return (Just N.AF_INET))
                <|> (string "TCP6" >> return (Just N.AF_INET6))
                <|> (string "UNKNOWN" >> return Nothing)


------------------------------------------------------------------------------
parseOldHaProxy :: Parser (Maybe (ByteString, Int, ByteString, Int, N.Family))
parseOldHaProxy = do
    string "PROXY "
    gotFamily <- parseFamily
    case gotFamily of
      Nothing  -> skipWhile (/= '\r') >> string "\r\n" >> return Nothing
      (Just f) -> do
          char ' '
          srcAddress <- takeWhile1 (/= ' ')
          char ' '
          destAddress <- takeWhile1 (/= ' ')
          char ' '
          srcPort <- decimal
          char ' '
          destPort <- decimal
          string "\r\n"
          return $! Just $! (srcAddress, srcPort, destAddress, destPort, f)


------------------------------------------------------------------------------
protocolHeader :: ByteString
protocolHeader = S8.pack [ 0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D
                         , 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A ]
{-# NOINLINE protocolHeader #-}


------------------------------------------------------------------------------
parseNewHaProxy :: N.SockAddr
                -> N.SockAddr
                -> N.SocketType
                -> Parser (N.SockAddr, N.SockAddr, N.Family, N.SocketType)
parseNewHaProxy origSrcAddr origDestAddr origSockType = do
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
    socketType <- toSocketType protocol

    addressLen <- ntohs <$> snarf16

    case () of
        !_ | command == 0x0 || family == 0x0 || protocol == 0x0   -- LOCAL
                -> handleLocal addressLen
           | family == 0x1 -> handleIPv4 addressLen socketType
           | family == 0x2 -> handleIPv6 addressLen socketType
           | family == 0x3 -> handleUnix addressLen socketType
           | otherwise     -> fail $ "Bad family " ++ show family

  where
    toSocketType 0 = return $! N.Stream
    toSocketType 1 = return $! N.Stream
    toSocketType 2 = return $! N.Datagram
    toSocketType _ = fail "bad protocol"

    handleLocal addressLen = do
        -- skip N bytes and return the original addresses
        when (addressLen > 500) $ fail $ "suspiciously long address "
                                          ++ show addressLen
        void $ take (fromIntegral addressLen)
        return $! ( origSrcAddr, origDestAddr, addrFamily origSrcAddr
                  , origSockType )

    handleIPv4 addressLen socketType = do
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
                  , N.AF_INET
                  , socketType
                  )

    handleIPv6 addressLen socketType = do
        let scopeId = 0   -- means "reserved", kludge alert!
        let flow    = 0

        when (addressLen < 36) $ fail $ "bad address length "
                                         ++ show addressLen
                                         ++ " for IPv6"
        let nskip = addressLen - 36
        s1 <- ntohl <$> snarf32
        s2 <- ntohl <$> snarf32
        s3 <- ntohl <$> snarf32
        s4 <- ntohl <$> snarf32

        d1 <- ntohl <$> snarf32
        d2 <- ntohl <$> snarf32
        d3 <- ntohl <$> snarf32
        d4 <- ntohl <$> snarf32

        sp <- snarf16
        dp <- snarf16

        void $ take $ fromIntegral nskip

        return $! ( N.SockAddrInet6 (N.PortNum sp) flow (s1, s2, s3, s4) scopeId
                  , N.SockAddrInet6 (N.PortNum dp) flow (d1, d2, d3, d4) scopeId
                  , N.AF_INET6
                  , socketType
                  )

    handleUnix addressLen socketType = do
        when (addressLen < 216) $ fail $ "bad address length "
                                         ++ show addressLen
                                         ++ " for unix"
        addr1 <- take 108
        addr2 <- take 108
        void $ take $ fromIntegral $ addressLen - 216
        return $! ( N.SockAddrUnix (toUnixPath addr1)
                  , N.SockAddrUnix (toUnixPath addr2)
                  , N.AF_UNIX
                  , socketType
                  )

    toUnixPath = S.unpack . fst . S.break (=='\x00')

foreign import ccall unsafe "iostreams_ntohs" c_ntohs :: CUShort -> CUShort
foreign import ccall unsafe "iostreams_ntohl" c_ntohl :: CUInt -> CUInt

ntohs :: Word16 -> Word16
ntohs = fromIntegral . c_ntohs . fromIntegral

ntohl :: Word32 -> Word32
ntohl = fromIntegral . c_ntohl . fromIntegral

snarf32 :: Parser Word32
snarf32 = do
    s <- take 4
    return $! unsafePerformIO $! S.unsafeUseAsCString s $ peek . castPtr


snarf16 :: Parser Word16
snarf16 = do
    s <- take 2
    return $! unsafePerformIO $! S.unsafeUseAsCString s $ peek . castPtr

addrFamily :: N.SockAddr -> N.Family
addrFamily s = case s of
                 (N.SockAddrInet _ _)      -> N.AF_INET
                 (N.SockAddrInet6 _ _ _ _) -> N.AF_INET6
                 (N.SockAddrUnix _ )       -> N.AF_UNIX
