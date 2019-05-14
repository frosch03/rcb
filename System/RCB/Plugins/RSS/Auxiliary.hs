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
import System.RCB.Room
import System.RCB.REST
import System.RCB.Auxiliary
import System.RCB.Configuration (rct_config_file, rct_jira_config_file)
import System.RCB.Plugins.RSS.Reader (readFeed, rss2string)
import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.RssConfig.FeedTransformer
import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import System.RCB.Plugins.RSS.RssConfig.Modifiers
import System.RCB.Plugins.RSS.ITransformable
import System.RCB.Plugins.REST.JiraConfig
import System.RCB.Plugins.REST.Auxiliary
import System.RCB.IRocketify
import System.RCB.IAscii


import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent       (threadDelay)
import Text.Read (readMaybe)
import Network.WebSockets.Connection (Connection(..))
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Control.Concurrent.MVar
import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Data.Text (Text, pack, unpack)
import System.Console.CmdArgs.GetOpt
import System.Console.CmdArgs.Explicit
    

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
    

cli :: (String -> IO ()) -> (MVar RssConfig, MVar JiraConfig) -> String -> IO Bool
cli notify (rssconfig, jiraconfig) s = do
    let amount = maybe 1 id $ countFromMsg s
    case (head . words $ s) of
      "exit" -> return True
 
      "add" -> do
        addCli (rssconfig, jiraconfig) . unwords . tail . words $ s
        updateCli notify rssconfig
        return False

      "del" -> do
        delCli (rssconfig, jiraconfig) . unwords . tail . words $ s
        readMVar rssconfig >>= notify . rctify
        return False

      "config" -> do
        readMVar rssconfig  >>= notify . rctify
        readMVar jiraconfig >>= notify . rctify
        return False 

      "rssconfig" -> do
        readMVar rssconfig >>= notify . rctify
        return False 

      "jiraconfig" -> do
        readMVar jiraconfig >>= notify . rctify
        return False 

      "update" -> do
        putStrLn "updateCli rssconfig"
        updateCli notify rssconfig
        putStrLn "updateCli jiraconfig"
        updateJiraCli notify jiraconfig
        return False

      "help" -> do
        notify . foldl (++) "" $ helpMsg
        return False

      "store" -> do
        mvrssconf <- readMVar rssconfig
        store rct_config_file mvrssconf
        mvjiraconf <- readMVar jiraconfig
        store rct_jira_config_file mvjiraconf
        return False

      "restore" -> do
        restore rssconfig
        restoreJira jiraconfig
        return False

      otherwise -> do
        cfg <- readMVar rssconfig
        sequence [ grepFeeds feed fns amount >>= mapM notify | (cmd, Feed feed fns) <- feeds cfg, cmd == (head . words $ s) ]
        return False
  
parseFeedTransformer :: String -> String -> FeedTransformer
parseFeedTransformer "title"       "empty"  = (FeedTransformer (Just Tempty) Nothing Nothing)
parseFeedTransformer "title" "drop_quotes"  = (FeedTransformer (Just Tdrop_quotes) Nothing Nothing)
parseFeedTransformer "title"       _        = (FeedTransformer Nothing Nothing Nothing)
parseFeedTransformer "link"        "empty"  = (FeedTransformer Nothing (Just Lempty) Nothing)
parseFeedTransformer "link"        _        = (FeedTransformer Nothing Nothing Nothing)
parseFeedTransformer "description" "empty"  = (FeedTransformer Nothing Nothing (Just Dempty))
parseFeedTransformer "description" "imgurl" = (FeedTransformer Nothing Nothing (Just Dimgurl))
parseFeedTransformer _             _        = (FeedTransformer Nothing Nothing Nothing)

add_feedtransformer_options :: [OptDescr FeedTransformer]
add_feedtransformer_options =
    [ Option ['t'] ["title"] (ReqArg (parseFeedTransformer "title") "function") "set the title transformer function"
    , Option ['l'] ["link"] (ReqArg (parseFeedTransformer "link") "function") "set the link transformer function"
    , Option ['d'] ["description"] (ReqArg (parseFeedTransformer "description") "function") "set the description transformer function"
    ]

parseOptions :: String -> FeedTransformer
parseOptions s = mconcat fts
    where
      (fts, _, _) = getOpt Permute add_feedtransformer_options . words $ s


addCli :: (MVar RssConfig, MVar JiraConfig) -> String -> IO ()
addCli (rssconfig, jiraconfig) s = do
  case (head . words $ s) of
    "command" -> do
      mvconf <- takeMVar rssconfig
      let fn s
              | (length . words $ s) > 1 = Just ((words s)!!0, (words s)!!1)
              | otherwise    = Nothing
          newconf = maybe mvconf (addRssCommand mvconf) (fn . unwords . tail . words $ s)
      putMVar rssconfig newconf
      store rct_config_file newconf

    "push" -> do
      mvconf <- takeMVar rssconfig
      let rst = unwords . tail . words $ s
          fn str
              | (length . words $ str) > 1 = Just ((Room ((words str)!!0) "" Direct), (words str)!!1)
              | otherwise    = Nothing
          ftr = parseOptions $ unwords . (drop 2) . words $ rst
          newconf = maybe mvconf (addPushToRoom_ mvconf ftr) (fn . unwords . tail . words $ s)
      putMVar rssconfig newconf
      store rct_config_file newconf

    "jql" -> do
      mvconf <- takeMVar jiraconfig
      let rst = unwords . tail . words $ s
          fn str
              | (length . words $ str) > 1 = Just ((Room ((words str)!!0) "" Direct), (words str)!!1)
              | otherwise    = Nothing
          newconf = maybe mvconf (addJqlToRoom mvconf) (fn . unwords . tail . words $ s)
      putStrLn $ show newconf
      putMVar jiraconfig newconf
      store rct_jira_config_file newconf

    otherwise -> return ()

delCli :: (MVar RssConfig, MVar JiraConfig) -> String -> IO ()
delCli (rssconfig, jiraconfig) s = do
  case (head . words $ s) of
    "command" -> do
      mvconf <- takeMVar rssconfig
      let fn s
              | (length . words $ s) > 0 = readMaybe $ (words s)!!0
              | otherwise    = Nothing
          newconf = maybe mvconf (delRssCommand mvconf) (fn . unwords . tail . words $ s)
      putMVar rssconfig newconf
      store rct_config_file newconf

    "push" -> do
      return ()
      mvconf <- takeMVar rssconfig
      let fn s
              | (length . words $ s) > 0 = readMaybe $ (words s)!!0
              | otherwise    = Nothing
          newconf = maybe mvconf (delPushToRoom mvconf) (fn . unwords . tail . words $ s)
      putMVar rssconfig newconf
      store rct_config_file newconf

    "jql" -> do
      return ()
      mvconf <- takeMVar jiraconfig
      let fn s
              | (length . words $ s) > 0 = readMaybe $ (words s)!!0
              | otherwise    = Nothing
          newconf = maybe mvconf (delJqlFromRoom mvconf) (fn . unwords . tail . words $ s)
      putMVar jiraconfig newconf
      store rct_jira_config_file newconf

    otherwise -> return ()

updateCli :: (String -> IO ()) -> MVar RssConfig -> IO ()
updateCli notify config = do
  forkIO $ do
    fillRSSRoomIDs config
    mvconf <- readMVar config
    store rct_config_file mvconf
    notify . rctify $ mvconf
  return ()

restore :: MVar RssConfig -> IO ()
restore rssconfig = do
  putStrLn $ "Restoring: rss configuration" 
  mvconf <- takeMVar rssconfig
  handle <- openFile rct_config_file ReadMode
  line <- hGetLine handle
  hClose handle
  putMVar rssconfig . read $ line
 
helpMsg :: [String]
helpMsg =
    [ "```"
    , "\\n"
    , "Help Commands\\n"
    , "=============\\n"
    , "\\n"
    , "add command <Keyword :: String> <URL :: String>     - add a new rss command\\n"
    , "add push <Room :: String> <URL :: String> [Options] - add a feed to be pushed to the room\\n"
    ] ++ pushOptionHelpMsg ++ 
    [ "add jql <Room :: String> <JQL :: String> - add a feed to be pushed to the room\\n"
    , "del command <Number :: Int>              - remove the rss command with the number\\n"
    , "del push <Number :: Int>                 - remove the push to room with the number\\n"
    , "del jql <Number :: Int>                  - remove the jql of room with the number\\n"
    , "config                                   - show both current configurations\\n"
    , "rssconfig                                - show the current rss configuration\\n"
    , "jiraconfig                               - show the current jira configuration\\n"
    , "update                                   - fill in the room id for new room entrys\\n"
    , "store                                    - storing the current configurations into local files\\n"
    , "restore                                  - restoring from local files the configurations \\n"
    , "exit                                     - shutdown the bot\\n"
    , "help                                     - display this screen\\n"
    , "\\n"
    , "```"
    ]
         
pushOptionHelpMsg :: [String]
pushOptionHelpMsg =
      drop 2
    . map (\x -> x ++ "\\n")
    . lines
    . concat
    . map (\x -> if x == '\\' then "\\\\" else [x])
    . show
    . helpText [] HelpFormatDefault
    . convert ""
    $ add_feedtransformer_options



grepData :: RssConfig -> Int -> IO [(String, [String])]
grepData cfg delay = do
    threadDelay . sec2µs $ delay
    let (io_feeds, rooms) =
            unzip [ (return . head =<< grepFeeds feed fns 1, map room_id rooms)
                        | (Feed feed fns, rooms) <- pushFeedIntoRoomss . pushs $ cfg ]
    feeds <- sequence io_feeds
    return $ zip feeds rooms
