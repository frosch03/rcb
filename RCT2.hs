module RCT2 where

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
import System.IO.Strict as IOS
import Control.Monad
import Control.Concurrent (threadDelay)
-- 

initialize :: Connection -> IO String
initialize c = do
  msg <- grepData
  putStrLn "initialized"
  return $ msg

sense :: Connection -> Bool -> IO (DTime, Maybe String)
sense c _ = do
  msg <- grepData
  return (0.0, Just msg)

actuate :: Connection -> Bool -> Maybe String -> IO Bool
actuate _ _ Nothing = return False
actuate c _ (Just msg) = do
  mid <- secondsOfTheDay
  let sendToRC = sendTextData c . pack . ascii . mkSendMsg mid rid
      rid = "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" -- TODO: Fieser Hack!
  sendToRC msg
  return False


grpNfeed :: Int -> String -> ((String -> String), (String -> String), (String -> String)) -> IO [String]
grpNfeed n feed (tFn, lFn, dFn) = do
  xs <- readFeed n feed
  let ts = map title       xs
      ls = map link        xs
      ds = map description xs
      ms = zipWith3 (\d t l -> "[" ++ (tFn t) ++ "](" ++ (lFn l) ++ "): " ++ (dFn d)) ds ts ls
  return ms
    
sec2µs :: (Integral i, RealFrac r) => r -> i
sec2µs = floor . (* 1E6)

grepData :: IO String
grepData = do
  let list = []
  threadDelay . sec2µs $ 10
  (contents:_) <- grpNfeed 1 "https://news.ycombinator.com/rss" (id, id, const "")
  -- (contents:_) <- grpNfeed 1 "https://blog.fefe.de/rss.xml?html" (id, id, const "") 
  -- handle <- openFile "/tmp/test.txt" ReadMode
  -- contents <- IOS.hGetContents handle
  -- hClose handle
  -- -- putStrLn contents
  return contents
