module RssCommands
    ( initialize
    , sense
    , process
    , actuate
    )
where

import Aux (secondsOfTheDay, countFromMsg)
import Ascii (ascii)
import Message (Message(..), ChangedField(..), ChangedFieldArgs(..), mkSendMsg)
import Method (RoomId)
import Configuration (init_string, login_string, subscribe)

import Plugin.RSS.Commands 
import Plugin.RSS.Auxiliary 

import FRP.Yampa (DTime, SF, arr)
import Data.Text (Text, pack, unpack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 


sendAndShow :: Connection -> Message -> IO ()
sendAndShow c s = do
    sendTextData c . pack . ascii $ s
    putStrLn $ show s

send :: Connection -> Message -> IO ()
send c =
    sendTextData c . pack . ascii

getText :: Message -> Maybe (String, String)
getText (Changed _ _ (CF _ ((CFA title text (_, rid, _, t, msg)):_))) =
    Just (rid, msg)
getText _ =
    Nothing
    

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

actuate :: Connection -> Bool -> Maybe (Method.RoomId, String) -> IO Bool
actuate c _ Nothing = return False
actuate c _ (Just (rid, msg)) = do
    mid <- secondsOfTheDay
    let sendMsgs :: [String] -> IO [()]
        sendMsgs = mapM $ send c . mkSendMsg mid rid
        amount   = maybe 1 id $ countFromMsg msg
    if (head . words $ msg) == "exit"
    then return True
    else sequence [ grepFeeds feed fns amount >>= sendMsgs | (cmd, Feed feed fns) <- feeds rssConfig, cmd == (head . words $ msg) ]
         >> return False
