module Main where

import Configuration

import Plugin.RSS.Commands

import RssCommands as RC
import RssPush     as RP

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Network.WebSockets (ClientApp)
import Wuss (runSecureClient)

import FRP.Yampa (reactimate)


reactiveWS :: ClientApp ()
reactiveWS c = do
  config <- newMVar rssConfig
  putStrLn "DISABLED: >>> Starting Continuous News Deliverer"
  putStrLn "Starting Continuous News Deliverer"
  void . forkIO . forever $ do
      reactimate (RP.initialize config c) (RP.sense config c) (RP.actuate c) RP.process

  putStrLn "Starting Rss Commands"
  reactimate (RC.initialize c) (RC.sense c) (RC.actuate config c) RC.process

main :: IO ()
main = runSecureClient rct_server rct_port rct_path reactiveWS

