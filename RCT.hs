module RCT where

import Aux (secondsOfTheDay)
import Ascii
import Message
import Method (RoomId)
import Configuration

import Plugin.RSS.Commands

import FRP.Yampa
import Data.Text (Text, pack, unpack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 

sendAndShow :: Connection -> Message -> IO ()
sendAndShow c s = do
    sendTextData c . pack . ascii $ s
    putStrLn $ show s

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
    -- putStrLn $ "> " ++ (unpack $ raw)
    let msg = read . unpack $ raw :: Message
    if (msg == Ping) 
       then (sendTextData c . pack . ascii $ Pong) >> return (0.0, Nothing)
       else return (0.0, Just msg)

actuate :: Connection -> Bool -> Maybe (Method.RoomId, String) -> IO Bool
actuate c _ Nothing = return False
actuate c _ (Just (rid, s)) = do
  let x = False
  mid <- secondsOfTheDay
  case (head . words $ s) of
    ("exit") -> return True    -- True means end
    ("fefe") -> lastNfefe (sendTextData c . pack . ascii . mkSendMsg mid rid) s >> return False
    ("xkcd") -> lastNxkcd (sendTextData c . pack . ascii . mkSendMsg mid rid) s >> return False
    otherwise -> putStrLn s >> return False
