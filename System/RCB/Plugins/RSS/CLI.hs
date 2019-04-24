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

import Data.Text (Text, pack, unpack)
import FRP.Yampa (DTime, SF, arr, loopPre)
import Control.Concurrent.MVar
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 


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
    -- putStrLn . unpack $ raw
    let msg = read . unpack $ raw :: Message
    if (msg == Ping) 
       then (send c $ Pong) >> return (0.0, Nothing)
       else return (0.0, Just msg)


actuate :: MVar RssConfig -> Connection -> Bool -> Maybe (RoomId, String) -> IO Bool
actuate _ c _ Nothing = return False
actuate config c _ (Just (rid, msg)) = do
    mid <- secondsOfTheDay
    let sendMsg :: String -> IO ()
        sendMsg = send c . mkSendMsg mid rid
    res <- cli sendMsg config msg
    return res
