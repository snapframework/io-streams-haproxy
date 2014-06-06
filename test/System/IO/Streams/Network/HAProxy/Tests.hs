{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Streams.Network.HAProxy.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Applicative                        ((<$>))
import qualified Control.Exception                          as E
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import qualified Data.ByteString                            as S8
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Char8                      as S
import           Data.Typeable
import qualified Network.Socket                             as N
import           System.IO.Streams                          (InputStream, OutputStream)
import qualified System.IO.Streams                          as Streams
import qualified System.IO.Streams.Network.HAProxy          as HA
import           System.IO.Streams.Network.Internal.Address (getSockAddr)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                                 hiding (Test)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testOldHaProxy
        , testOldHaProxy6
        , testOldHaProxyFailure
        , testOldHaProxyLocal
        , testNewHaProxy
        , testTrivials
        ]


------------------------------------------------------------------------------
runInput :: ByteString
         -> N.SockAddr
         -> N.SockAddr
         -> (HA.ProxyInfo -> InputStream ByteString -> OutputStream ByteString -> IO a)
         -> IO a
runInput input sa sb action = do
    is <- Streams.fromList [input]
    (os, _) <- Streams.listOutputStream
    HA.behindHAProxyWithAddresses (sa, sb) (is, os) action


------------------------------------------------------------------------------
testOldHaProxy :: Test
testOldHaProxy = testCase "test/old_ha_proxy" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    runInput "PROXY TCP4 127.0.0.1 127.0.0.1 10000 80\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 10000
        sb <- localhost 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxy6 :: Test
testOldHaProxy6 = testCase "test/old_ha_proxy6" $ do
    sa <- localhost6 1111
    sb <- localhost6 2222
    runInput "PROXY TCP6 ::1 ::1 10000 80\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost6 10000
        sb <- localhost6 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxyLocal :: Test
testOldHaProxyLocal = testCase "test/old_ha_proxy_local" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    runInput "PROXY UNKNOWN\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 1111
        sb <- localhost 2222
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxyFailure :: Test
testOldHaProxyFailure = testCase "test/old_ha_proxy_failure" $ do
     sa <- localhost 1111
     sb <- localhost 2222
     expectException "bad family"
       $ runInput "PROXY ZZZ qqqq wwww 10000 80\r\nblah" sa sb action
     expectException "short"
       $ runInput "PROXY TCP4 \r\nblah" sa sb action
     expectException "non-integral"
       $ runInput "PROXY TCP4 127.0.0.1 127.0.0.1 xxx yyy\r\nblah" sa sb action
  where
    action _ _ _ = return ()


------------------------------------------------------------------------------
protocolHeader :: ByteString
protocolHeader = S8.pack [ 0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D
                         , 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A ]
{-# NOINLINE protocolHeader #-}


------------------------------------------------------------------------------
testNewHaProxy :: Test
testNewHaProxy = testCase "test/new_ha_proxy" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x21\x11"    -- TCP over v4
                         , "\x00\x0c"        -- 12 bytes of address (network ordered)
                         , "\x7f\x00\x00\x01" -- localhost
                         , "\x7f\x00\x00\x01"
                         , "\x27\x10" -- 10000 in network order
                         , "\x00\x50" -- 80 in network order
                         , "blah"     -- the rest
                         ]
    runInput input sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 10000
        sb <- localhost 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "test/trivials" $ do
    coverTypeableInstance $ HA.makeProxyInfo undefined undefined
    coverShowInstance $ HA.makeProxyInfo undefined undefined


------------------------------------------------------------------------------
localhost :: Int -> IO (N.SockAddr)
localhost p = N.SockAddrInet (fromIntegral p) <$> N.inet_addr "127.0.0.1"


------------------------------------------------------------------------------
localhost6 :: Int -> IO (N.SockAddr)
localhost6 p = snd <$> getSockAddr p "::1"


------------------------------------------------------------------------------
expectException :: String -> IO a -> IO ()
expectException name act = do
    e <- E.try act
    case e of
      Left (z::E.SomeException) -> (length $ show z) `seq` return ()
      Right _ -> fail $ name ++ ": expected exception, didn't get one"

------------------------------------------------------------------------------
coverTypeableInstance :: (Monad m, Typeable a) => a -> m ()
coverTypeableInstance a = typeOf a `seq` return ()


------------------------------------------------------------------------------
eatException :: IO a -> IO ()
eatException a = (a >> return ()) `E.catch` handler
  where
    handler :: E.SomeException -> IO ()
    handler _ = return ()


------------------------------------------------------------------------------
-- | Kill the false negative on derived show instances.
coverShowInstance :: (MonadIO m, Show a) => a -> m ()
coverShowInstance x = liftIO (a >> b >> c)
  where
    a = eatException $ E.evaluate $ showsPrec 0 x ""
    b = eatException $ E.evaluate $ show x
    c = eatException $ E.evaluate $ showList [x] ""
