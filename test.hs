import Wuss

import Ascii
import Algo
import Method
import Message

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void, when)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Data.List (isInfixOf)


main :: IO ()
main = runSecureClient "c.frosch03.de" 443 "/websocket/" ws

init_string  = Con 1 1 
login_string = Mtd (Login "lambdabot" "1xunil3" SHA256) 
subscribe    = SubStreamNotifyUser 23 "5gBGjzg9oHMZb9DpR"
unsubscribe  = Nosub 23

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    putStrLn $ show init_string
    putStrLn $ show login_string

    void . forkIO . forever $ do
        message <- receiveData connection
        if ("ping" `isInfixOf` (show message))
           then sendTextData connection . pack . ascii $ Pong
           else print (message :: Text)

    sendTextData connection . pack . ascii $ init_string
    sendTextData connection . pack . ascii $ login_string
    -- sendTextData connection . pack . ascii $ subscribe
    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection . pack $ line
                loop
    loop

    putStrLn "Bye!"
    sendClose connection . pack $ "Bye!"



-- Sample Msg
-- { "msg":"changed"
-- , "collection":"stream-notify-user"
-- , "id":"id"
-- , "fields":
--   { "eventName":"5gBGjzg9oHMZb9DpR/notification"
--   , "args":
--     [ { "title":"@admin"
--       , "text":"test"
--       , "payload":
--         { "_id":"h4c9fvZhvqNLu2bSR"
--         , "rid":"5gBGjzg9oHMZb9DpRoH9xE5Zs4mvSByHm3"
--         , "sender":
--           { "_id":"oH9xE5Zs4mvSByHm3"
--           , "username":"admin"
--           , "name":"Administrator"
--           }
--         , "type":"d"
--         , "message":
--           { "msg":"test"
--           } } } ] } }

-- "{\"server_id\":\"0\"}"
 -- "{\"msg\":\"connected\",\"session\":\"tmhgQWp8rzkyHGjGA\"}"
-- "{\"msg\":\"added\",\"collection\":\"users\",\"id\":\"dNfBQiWGorDmHwWXR\",\"fields\":{\"username\":\"lambdabot\",\"emails\":[{\"address\":\"brettschneider@frosch03.de\",\"verified\":true}]}}"
-- "{\"msg\":\"result\",\"id\":\"23\",\"result\":{\"id\":\"dNfBQiWGorDmHwWXR\",\"token\":\"mRwPwVYV1e6TNLT-ym4w6NJQs70u_ZpmkGWXHw4YRHL\",\"tokenExpires\":{\"$date\":1559883842094},\"type\":\"password\"}}"
-- "{\"msg\":\"updated\",\"methods\":[\"23\"]}"
              
