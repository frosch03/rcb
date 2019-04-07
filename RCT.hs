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

-- Temp
import Plugin.RSS.Reader (readFeed, title, link, description)
import System.IO
import Control.Monad
-- 

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
actuate c _ (Just (rid, msg)) = do
  mid <- secondsOfTheDay
  let sendToRC = sendTextData c . pack . ascii . mkSendMsg mid rid
  case (head . words $ msg) of
    ("exit") -> return True    -- True means end
    ("fefe") -> lastNfefe sendToRC msg >> return False
    ("xkcd") -> lastNxkcd sendToRC msg >> return False
    ("heise") -> lastNheise sendToRC msg >> return False
    ("golem") -> lastNgolem sendToRC msg >> return False
    ("hackernews") -> lastNhackernews sendToRC msg >> return False
    otherwise -> putStrLn msg >> return False


tstFefe = grpNfeed 1 "https://blog.fefe.de/rss.xml?html" (id, id, const "") 
          
grpNfeed :: Int -> String -> ((String -> String), (String -> String), (String -> String)) -> IO [String]
grpNfeed n feed (tFn, lFn, dFn) = do
  xs <- readFeed n feed
  let ts = map title       xs
      ls = map link        xs
      ds = map description xs
      ms = zipWith3 (\d t l -> "[" ++ (tFn t) ++ "](" ++ (lFn l) ++ "): " ++ (dFn d)) ds ts ls
  return ms
    
                
showFile = do
  let list = []
  handle <- openFile "/tmp/test.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle
  return contents
