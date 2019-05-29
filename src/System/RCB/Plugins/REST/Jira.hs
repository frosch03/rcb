-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.REST.Ira
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  inoperable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.REST.Jira
where

import System.RCB.Configuration
import System.RCB.Auxiliary (secondsOfTheDay, changed)
import System.RCB.IAscii
import System.RCB.Room
import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Method (RoomId)
import Data.RocketChat.Message.Constructors
import System.RCB.Plugins.REST.Auxiliary (grepJiraData)
import System.RCB.Plugins.REST.JiraConfig

import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import System.RCB.Plugins.RSS.RssConfig.Datatype

import FRP.Yampa (DTime, SF, arr, loopPre)
import Data.Text (Text, pack, unpack)
import Control.Concurrent.MVar
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 
import Control.Monad (when)

process :: SF [(Bool, (String, [String]))] [(Bool, (String, [String]))]
process = changed

initialize :: MVar JiraConfig -> Connection -> IO [(Bool, (String, [String]))]
initialize config c = do
    cfg <- readMVar config
    datas <- grepJiraData cfg 0
    putStrLn "JIRA: initialized"
    return $ map (\x -> (False, x)) datas

sense :: MVar JiraConfig -> Connection -> Bool -> IO (DTime, Maybe [(Bool, (String, [String]))])
sense config c _ = do
    cfg <- readMVar config
    datas <- grepJiraData cfg (jiraInterval cfg)
    return (0.0, Just $ map (\x -> (False, x)) datas)

actuate :: MVar Bool -> (MVar Bool,MVar Bool,MVar Bool) -> Connection -> Bool -> [(Bool, (String, [String]))] -> IO Bool
actuate exit (_,exitted,_) c _ datas = do
    ext <- readMVar exit
    mid <- secondsOfTheDay
    let sendToRC = (\r -> sendTextData c . pack . ascii . mkSendMsg mid r)
    sequence . concat $ [ map (flip sendToRC $ msg) rids
                              | (equal, (msg, rids)) <- datas, not equal ]
    when ext $ do
      putStrLn "JIRA: Shutting down"
      takeMVar exitted >> putMVar exitted True
    return ext
