module System.RCB.Plugins.RSS.Push
where

import System.RCB.Auxiliary
import System.RCB.Plugins.RSS.RssConfig.Datatype
import Data.RocketChat.Message.Constructors
import System.RCB.IAscii
import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import System.RCB.Plugins.RSS.Auxiliary
    
import Data.Text (Text, pack, unpack)
import Control.Concurrent.MVar
import FRP.Yampa (DTime, SF, arr, loopPre)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection (Connection(..)) 


process :: SF [(Bool, (String, [String]))] [(Bool, (String, [String]))]
process =
    changed

initialize :: MVar RssConfig -> Connection -> IO [(Bool, (String, [String]))]
initialize config c = do
    cfg <- readMVar config
    datas <- grepData cfg 0
    putStrLn "initialized"
    return $ map (\x -> (False, x)) datas

sense :: MVar RssConfig -> Connection -> Bool -> IO (DTime, Maybe [(Bool, (String, [String]))])
sense config c _ = do
    cfg <- readMVar config
    datas <- grepData cfg (pushInterval . pushs $ cfg)
    return (0.0, Just $ map (\x -> (False, x)) datas)

actuate :: Connection -> Bool -> [(Bool, (String, [String]))] -> IO Bool
actuate c _ datas = do
    mid <- secondsOfTheDay
    let sendToRC = (\r -> sendTextData c . pack . ascii . mkSendMsg mid r)
    sequence . concat $ [ map (flip sendToRC $ msg) rids
                              | (equal, (msg, rids)) <- datas, not equal ]
    return False
