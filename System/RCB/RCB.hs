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
import System.RCB.Plugins.RSS.Configuration
import System.RCB.Plugins.RSS.CLI as CLI
import System.RCB.Plugins.RSS.Push as PUSH

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Network.WebSockets (ClientApp)
import Wuss (runSecureClient)
import FRP.Yampa (reactimate)

reactiveWS :: ClientApp ()
reactiveWS c = do
  config <- newMVar rssConfig
  -- putStrLn "DISABLED: >>> Starting Continuous News Deliverer"
  putStrLn "Starting Continuous News Deliverer"
  void . forkIO . forever $ do
      reactimate (PUSH.initialize config c) (PUSH.sense config c) (PUSH.actuate c) PUSH.process

  putStrLn "Starting Rss Commands"
  reactimate (CLI.initialize config c) (CLI.sense c) (CLI.actuate config c) CLI.process

main :: IO ()
main = runSecureClient rct_server rct_port rct_path reactiveWS

    
