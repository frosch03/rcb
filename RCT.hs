module RCT where

import Aux (secondsOfTheDay)
import Ascii
import Message
import Method (RoomId)
import Configuration
import Plugin.RssReader.Reader (readFeed, title, link, description)

import FRP.Yampa
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

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
    -- putStrLn $ "> " ++ (unpack $ raw)
    let msg = read . unpack $ raw :: Message
    if (msg == Ping) 
       then (sendTextData c . pack . ascii $ Pong) >> return (0.0, Nothing)
       else return (0.0, Just msg)

actuate :: Connection -> Bool -> Maybe (Method.RoomId, String) -> IO Bool
actuate c _ Nothing = return False
actuate c _ (Just (rid, s)) = do
  let x = False
  case (head . words $ s) of
    ("exit") -> return True    -- True means end
    ("fefe") -> lastNfefe c rid s >> return False
    ("xkcd") -> lastNxkcd c rid s >> return False
    otherwise -> putStrLn s >> return False



grepImgUrl :: String -> String
grepImgUrl s = fst $
    case (parse pImageTag' "" s) of
      Left err  -> error $ show err
      Right xs  -> xs
    where 
      pImageTag' :: GenParser Char st (String, String)
      pImageTag' = do
        many $ noneOf "<"
        string "<img src=\""
        src <- manyTill anyChar (try $ string "\" title")
        string "=\""
        title <- manyTill anyChar (try $ string "\" alt")
        string "=\""
        alt <- manyTill anyChar (try $ string "\" />")
        many anyChar
        return (src, title)

lastNxkcd :: Connection -> Method.RoomId -> String -> IO ()
lastNxkcd c rid s = do
  let n = if (length . tail . words $ s) > 0
          then maybe 1 id (readMaybe (head . tail . words $ s) :: Maybe Int)
          else 1
  xs <- readFeed n "https://www.xkcd.com/rss.xml"
  let ts = map title       xs
      ls = map link        xs
      ds = map description xs
      ms = zipWith3 (\d t l -> "[" ++ t ++ "](" ++ l ++ "): " ++ (grepImgUrl d)) ds ts ls
  mid <- secondsOfTheDay
  mapM (\m -> putStrLn m) ms
  mapM (\m -> sendTextData c . pack . ascii $ mkSendMsg mid rid m) ms
  return ()


lastNfefe :: Connection -> Method.RoomId -> String -> IO ()
lastNfefe c rid s = do
  let n = if (length . tail . words $ s) > 0
          then maybe 1 id (readMaybe (head . tail . words $ s) :: Maybe Int)
          else 1
  xs <- readFeed n "http://blog.fefe.de/rss.xml?html"
  let ts = map title xs
      ls = map link  xs
      ms = zipWith (\t l -> "[" ++ t ++ "](" ++ l ++ ")") ts ls
  mid <- secondsOfTheDay
  mapM (\m -> putStrLn m) ms
  mapM (\m -> sendTextData c . pack . ascii $ mkSendMsg mid rid m) ms
  return ()

