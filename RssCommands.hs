module RssCommands
    ( initialize
    , sense
    , process
    , actuate
    )
where

import RCTRest (fillRoomIDs)

import Aux (secondsOfTheDay, countFromMsg)
import Ascii (ascii)
import Message (Message(..), ChangedField(..), ChangedFieldArgs(..), mkSendMsg)
import Method (RoomId)
import Configuration (init_string, login_string, subscribe)

import Plugin.RSS.Commands (RssConfig, FeedDescriptor(..), feeds, rctify, addRssCommand, addPushToRoom, delRssCommand, delPushToRoom, Room(..), RoomType(..)) 
import Plugin.RSS.Auxiliary 

import FRP.Yampa (DTime, SF, arr)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar


process :: SF Message (Maybe (String, String))
process =
    arr $ getText

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
       then (send c $ Pong) >> return (0.0, Nothing)
       else return (0.0, Just msg)


actuate :: MVar RssConfig -> Connection -> Bool -> Maybe (Method.RoomId, String) -> IO Bool
actuate _ c _ Nothing = return False
actuate config c _ (Just (rid, msg)) = do
    mid <- secondsOfTheDay
    let sendMsg :: String -> IO ()
        sendMsg = send c . mkSendMsg mid rid
    res <- cli sendMsg config msg
    return res
               

sendAndShow :: Connection -> Message -> IO ()
sendAndShow c s = do
    send c s
    putStrLn $ show s

send :: Connection -> Message -> IO ()
send c =
    sendTextData c . pack . ascii

getText :: Message -> Maybe (String, String)
getText (Changed _ _ (CF _ ((CFA title text (_, rid, _, t, msg)):_))) =
    Just (rid, msg)
getText _ =
    Nothing
    

cli :: (String -> IO ()) -> MVar RssConfig -> String -> IO Bool
cli notify config s = do
    let amount = maybe 1 id $ countFromMsg s
    case (head . words $ s) of
      "exit" -> return True
 
      "add" -> do
        addCli config . unwords . tail . words $ s
        return False

      "del" -> do
        delCli config . unwords . tail . words $ s
        return False

      "config" -> do
        cfg <- readMVar config
        notify . rctify $ cfg
        return False 

      "update" -> do
        forkIO . fillRoomIDs $ config
        cfg <- readMVar config
        notify . rctify $ cfg
        return False

      "help" -> do
        notify . foldl (++) "" $ helpMsg
        return False

      otherwise -> do
        cfg <- readMVar config
        sequence [ grepFeeds feed fns amount >>= mapM notify | (cmd, Feed feed fns) <- feeds cfg, cmd == (head . words $ s) ]
        return False
  


addCli :: MVar RssConfig -> String -> IO ()
addCli config s = do
  case (head . words $ s) of
    "command" -> do
      mvconf <- takeMVar config
      let fn s
              | (length . words $ s) > 1 = Just ((words s)!!0, (words s)!!1)
              | otherwise    = Nothing
          newconf = maybe mvconf (addRssCommand mvconf) (fn . unwords . tail . words $ s)
      putMVar config newconf

    "push" -> do
      return ()
      mvconf <- takeMVar config
      let fn s
              | (length . words $ s) > 1 = Just ((Room ((words s)!!0) "" Direct), (words s)!!1)
              | otherwise    = Nothing
          newconf = maybe mvconf (addPushToRoom mvconf) (fn . unwords . tail . words $ s)
      putMVar config newconf

    otherwise -> return ()

delCli :: MVar RssConfig -> String -> IO ()
delCli config s = do
  case (head . words $ s) of
    "command" -> do
      mvconf <- takeMVar config
      let fn s
              | (length . words $ s) > 0 = readMaybe $ (words s)!!0
              | otherwise    = Nothing
          newconf = maybe mvconf (delRssCommand mvconf) (fn . unwords . tail . words $ s)
      putMVar config newconf

    "push" -> do
      return ()
      mvconf <- takeMVar config
      let fn s
              | (length . words $ s) > 0 = readMaybe $ (words s)!!0
              | otherwise    = Nothing
          newconf = maybe mvconf (delPushToRoom mvconf) (fn . unwords . tail . words $ s)
      putMVar config newconf

    otherwise -> return ()

helpMsg :: [String]
helpMsg =
    [ "```"
    , "\\n"
    , "Help Commands\\n"
    , "=============\\n"
    , "\\n"
    , "add command <Keyword :: String> <URL :: String> - add a new rss command\\n"
    , "add push <Room :: String> <URL :: String>       - add a feed to be pushed to the room\\n"
    , "del command <Number :: Int>                     - remove the rss command with the number\\n"
    , "del push <Number :: Int>                        - remove the push to room with the number\\n"
    , "config                                          - show the current configuration\\n"
    , "update                                          - fill in the room id for new room entrys\\n"
    , "exit                                            - shutdown the bot\\n"
    , "help                                            - display this screen\\n"
    , "\\n"
    , "```"
    ]
