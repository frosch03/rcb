-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.Auxiliary
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.Auxiliary where

import Data.RocketChat.Message
import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.ChangedField
import Data.RocketChat.Message.ChangedField.ChangedFieldArgs
import System.RCB.Auxiliary
import System.RCB.Plugins.RSS.Reader (readFeed, rss2string)
import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.RssConfig.FeedTransformer
import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import System.RCB.Plugins.RSS.RssConfig.Modifiers
import System.RCB.Plugins.RSS.IRocketify
import System.RCB.REST
import System.RCB.IAscii

import Control.Concurrent (forkIO)
import Control.Concurrent       (threadDelay)
import Text.Read (readMaybe)
import Network.WebSockets.Connection (Connection(..))
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Control.Concurrent.MVar
import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Data.Text (Text, pack, unpack)
    
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

grepFeeds :: String -> FeedTransformer -> Int -> IO [String]
grepFeeds feed ftr i = do
  xs <- readFeed i feed
  let ms = map (rss2string ftr) xs
  return ms

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
         

grepData :: RssConfig -> Int -> IO [(String, [String])]
grepData cfg delay = do
    threadDelay . sec2µs $ delay
    let (io_feeds, rooms) =
            unzip [ (return . head =<< grepFeeds feed fns 1, map room_id rooms)
                        | (Feed feed fns, rooms) <- pushFeedIntoRoomss . pushs $ cfg ]
    feeds <- sequence io_feeds
    return $ zip feeds rooms
