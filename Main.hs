module Main where

import RCT
import StreamFunctions
import Configuration

import Wuss
import FRP.Yampa
import Network.WebSockets (ClientApp)

reactiveWS :: ClientApp ()
reactiveWS c = do
  putStrLn "Starting Yampa"
  reactimate (initialize c) (sense c) (actuate c) process

main :: IO ()
main = runSecureClient rct_server rct_port rct_path reactiveWS

