-------------------------------------------------------------------------------
-- | Module      :  System.RCB.RCB
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
--   Connect to a rocket chat server via
--   <https://hackage.haskell.org/package/wuss web-sockets> and
--   read/write from configured channels.
-- 
--   Configuration files are within:
-- 
--   - the RSS plugins config within
--     @System/RCB/Plugins/RSS/Configuration.lhs@ 
--   - a global one within @System/RCB/Configuration@
--
-------------------------------------------------------------------------------

module System.RCB.RCB
where

import Data.RocketChat.Message
import System.RCB.Configuration
import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.Configuration
import System.RCB.Plugins.RSS.CLI as CLI
import System.RCB.Plugins.RSS.Push as PUSH

import GHC.IO.Handle.Text
import GHC.IO.Handle.FD
import GHC.IO.IOMode
import Control.Exception
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Network.WebSockets --(ClientApp)
import Network.Connection
import Wuss --(runSecureClient)
import FRP.Yampa (reactimate)
import Data.ByteString.Lazy (toStrict)
import Network.WebSockets.Stream (makeStream)

reactiveWS :: ClientApp ()
reactiveWS c = do
  cfg <- readConfigFrom rct_config_file `catch` returnDefaultConfig
  logReadConfigFrom rct_config_file `catch` logReturnDefaultConfig
  config <- newMVar cfg
  -- putStrLn "DISABLED: >>> Starting Continuous News Deliverer"
  putStrLn "Starting Continuous News Deliverer"
  void . forkIO . forever $ do
      reactimate (PUSH.initialize config c) (PUSH.sense config c) (PUSH.actuate c) PUSH.process

  putStrLn "Starting Rss Commands"
  reactimate (CLI.initialize config c) (CLI.sense c) (CLI.actuate config c) CLI.process

    where
      readConfigFrom cfg  = (openFile cfg ReadMode) >>= hGetLine >>= (return . read)
      returnDefaultConfig = (return . (\e -> const rssConfig (e :: IOException)))
      logReadConfigFrom cfg  = (openFile cfg ReadMode) >> (putStrLn $ "Restoring RssConfig from file: " ++ cfg)
      logReturnDefaultConfig = (\e -> const (putStrLn "Using default Configuration") (e :: IOException))


options = defaultConnectionOptions
headers = []
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = True
    , settingDisableSession = False
    , settingUseServerName  = False
    }
connectionParams = ConnectionParams
    { connectionHostname  = rct_server
    , connectionPort      = rct_port
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks  = Nothing
    }
    
main :: IO ()
-- main = runSecureClient rct_server rct_port rct_path reactiveWS
main = do
  context <- initConnectionContext
  connection <- connectTo context connectionParams
  stream <- makeStream
      (fmap Just (connectionGetChunk connection))
      (maybe (return ()) (connectionPut connection . toStrict))
  runClientWithStream stream rct_server rct_path options headers reactiveWS
