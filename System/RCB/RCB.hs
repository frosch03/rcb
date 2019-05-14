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
--   - the RSS plugins mvRssConfig within
--     @System/RCB/Plugins/RSS/Configuration.lhs@ 
--   - a global one within @System/RCB/Configuration@
--
-------------------------------------------------------------------------------

module System.RCB.RCB
where

import Data.RocketChat.Message
import System.RCB.Configuration

import System.RCB.Auxiliary (sec2µs)
import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.Configuration
import System.RCB.Plugins.RSS.CLI as CLI
import System.RCB.Plugins.RSS.Push as PUSH

import System.RCB.Plugins.REST.Configuration
import System.RCB.Plugins.REST.Jira as JIRA

import System.IO
import GHC.IO.Handle.Text
import GHC.IO.Handle.FD
import GHC.IO.IOMode
import Control.Exception
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Network.WebSockets --(ClientApp)
import Network.Connection
import Wuss --(runSecureClient)
import FRP.Yampa (reactimate)
import Data.ByteString.Lazy (toStrict)
import Network.WebSockets.Stream (makeStream)


forking :: IO () -> IO b -> IO b
forking act k = do tid <- forkIO act
                   k `finally` killThread tid
    
reactiveWS :: ClientApp ()
reactiveWS c = do
  exitRequest <- newMVar False
  mvMainEnded <- newMVar False
  mvJiraEnded <- newMVar False
  mvPushEnded <- newMVar False
  isMainEnded <- readMVar mvMainEnded
  isJiraEnded <- readMVar mvJiraEnded
  isPushEnded <- readMVar mvPushEnded
  let endedStates = (mvMainEnded, mvJiraEnded, mvPushEnded)
  -- putStrLn "DISABLED: >>> Starting Continuous News Deliverer"
  -- putStrLn "Starting Continuous News Deliverer"
  rssConfig  <- readConfigFrom rct_config_file `catch` returnDefaultConfig
  logReadConfigFrom rct_config_file `catch` logReturnDefaultConfig
  mvRssConfig  <- newMVar rssConfig
  ptid <- forkIO $
         reactimate
           (PUSH.initialize mvRssConfig c)
           (PUSH.sense mvRssConfig c)
           (PUSH.actuate exitRequest endedStates c)
           (PUSH.process)

  -- putStrLn "Starting Jira Pushs"
  jiraConfig <- readConfigFrom rct_jira_config_file `catch` returnJiraDefaultConfig
  logReadConfigFrom rct_jira_config_file `catch` logReturnDefaultConfig
  mvJiraConfig <- newMVar jiraConfig
  jtid <- forkIO $
         reactimate
           (JIRA.initialize mvJiraConfig c)
           (JIRA.sense mvJiraConfig c)
           (JIRA.actuate exitRequest endedStates c)
           (JIRA.process)

  -- putStrLn "Starting Rss Commands"
  reactimate
    (CLI.initialize mvRssConfig c)
    (CLI.sense c)
    (CLI.actuate exitRequest endedStates (mvRssConfig, mvJiraConfig) c)
    (CLI.process)

  putStrLn $ "MAIN: Requesting all Plugins Shutdown"
  takeMVar exitRequest >> putMVar exitRequest True

  putStrLn $ "MAIN: Waiting for other Plugins"
  waitForChilds isJiraEnded isPushEnded
  
  putStrLn "MAIN Shutting down"
  -- killThread ptid
  -- killThread jtid
      
    where
      waitForChilds eJ eR = do
                 threadDelay . sec2µs $ 1
                 if eJ && eR
                 then return ()
                 else waitForChilds eJ eR
      readConfigFrom rssConfig  = do
                 handle <- openFile rssConfig ReadMode
                 line <- hGetLine handle
                 hClose handle
                 return . read $ line
      returnDefaultConfig     = (return . (\e -> const rssConfig (e :: IOException)))
      returnJiraDefaultConfig = (return . (\e -> const jiraConf  (e :: IOException)))
      logReadConfigFrom filename  = (openFile filename ReadMode) >>= hClose >> (putStrLn $ "MAIN: Restoring RssConfig from file: " ++ filename)
      logReturnDefaultConfig = (\e -> const (putStrLn "MAIN: Using default Configuration") (e :: IOException))


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
