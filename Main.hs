module Main where

import RCT as R1
import StreamFunctions as S1
import RCT2 as R2
import StreamFunctions2 as S2
import Configuration

import Wuss
import FRP.Yampa
import Network.WebSockets (ClientApp)

-- NEW
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
-- --

reactiveWS :: ClientApp ()
reactiveWS c = do
  putStrLn "Starting Continuous News Deliverer"
  void . forkIO . forever $ do
      reactimate (R2.initialize c) (R2.sense c) (R2.actuate c) S2.process

  putStrLn "Starting Yampa"
  reactimate (R1.initialize c) (R1.sense c) (R1.actuate c) S1.process

main :: IO ()
main = runSecureClient rct_server rct_port rct_path reactiveWS

