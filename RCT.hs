module RCT where

import Ascii
import Message
import Configuration

import FRP.Yampa
import Data.Text (Text, pack, unpack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 


sendAndShow :: Connection -> Message -> IO ()
sendAndShow c s = do
    sendTextData c . pack . ascii $ s
    putStrLn $ show s

-- sendAndShows :: Connection -> [Message] -> IO [a]
-- sendAndShows c = mapM (sendAndShow c) 

initialize :: Connection -> IO Message
initialize c = do
    mapM (sendAndShow c)
      [ init_string
      , login_string
      , subscribe
      ]
    msg <- receiveData c
    return $ read . unpack $ msg

sense :: Connection -> Bool -> IO (DTime, Maybe Message)
sense c _ = do
    raw <- receiveData c
    putStrLn (unpack $ raw)
    let msg = read . unpack $ raw :: Message
    if (msg == Ping) 
       then (sendTextData c . pack . ascii $ Pong) >> return (0.0, Nothing)
       else return (0.0, Just msg)

actuate :: Connection -> Bool -> Maybe String -> IO Bool
actuate _ _ Nothing = return False
actuate c _ (Just s) = do
    if (s == "exit")
       then return True   -- True means end
       else putStrLn s >> return False
