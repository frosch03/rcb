-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.CLI
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.CLI
where

import System.RCB.Configuration
import System.RCB.Auxiliary
import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.Auxiliary
import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Method (RoomId)
import Data.RocketChat.Message.Constructors
import System.RCB.Plugins.RSS.Configuration

import System.RCB.Plugins.REST.JiraConfig

import Data.Text (Text, pack, unpack)
import FRP.Yampa (DTime, SF, arr, loopPre)
import Control.Concurrent.MVar
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 
import Control.Monad (when)


process :: SF Message (Maybe (String, String))
process =
    arr $ getText

initialize :: MVar RssConfig -> Connection -> IO Message
initialize rssconfig c = do
    mapM (sendAndShow c)
      [ init_string
      , login_string
      , subscribe
      ]
    msg <- receiveData c
    putStrLn "CLI:  initialized"
    return $ read . unpack $ msg

sense :: Connection -> Bool -> IO (DTime, Maybe Message)
sense c _ = do
    raw <- receiveData c
    -- putStrLn . unpack $ raw
    let msg = read . unpack $ raw :: Message
    if (msg == Ping) 
       then (send c $ Pong) >> return (0.0, Nothing)
       else return (0.0, Just msg)


actuate :: MVar Bool -> (MVar Bool,MVar Bool,MVar Bool) -> (MVar RssConfig, MVar JiraConfig) -> Connection -> Bool -> Maybe (RoomId, String) -> IO Bool
actuate _ _ _ c _ Nothing = return False
actuate exit (exitted,exittedJ,exittedR) configs c _ (Just (rid, msg)) = do
    ext <- readMVar exit
    mid <- secondsOfTheDay
    let sendMsg :: String -> IO ()
        sendMsg = send c . mkSendMsg mid rid
    res <- cli sendMsg configs msg
    when res $ do
      putStrLn $ "CLI:  Shutting down"
      takeMVar exitted >> putMVar exitted True
    return res
        
