module Main where

import Configuration

import RssCommands as RC
import RssPush     as RP

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Network.WebSockets (ClientApp)
import Wuss (runSecureClient)

import FRP.Yampa (reactimate)


reactiveWS :: ClientApp ()
reactiveWS c = do
  putStrLn "Starting Continuous News Deliverer"
  void . forkIO . forever $ do
      reactimate (RP.initialize c) (RP.sense c) (RP.actuate c) RP.process

  putStrLn "Starting Rss Commands"
  reactimate (RC.initialize c) (RC.sense c) (RC.actuate c) RC.process

main :: IO ()
main = runSecureClient rct_server rct_port rct_path reactiveWS

