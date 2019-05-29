-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message
where

import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Instances
import Data.RocketChat.Message.Parser
import Data.RocketChat.Message.Write

-- |-   Messages
-- |    -   connected
-- |        \"{\\\"msg\\\":\\\"connected\\\",\\\"session\\\":\\\"RGKknraTEmHoaE4MP\\\"}\"
-- |        [session]
-- |            String
-- |
-- |    -   ready
-- |        \"{\\\"msg\\\":\\\"ready\\\",\\\"subs\\\":[\\\"23\\\"]}\"
-- |        [subs]
-- |            [Int]
-- |
-- |    -   updated
-- |        \"{\\\"msg\\\":\\\"updated\\\",\\\"methods\\\":[\\\"42\\\"]}\"
-- |        [methods]
-- |            [Int]
-- |
-- |    -   nosub
-- |        \"{\\\"msg\\\":\\\"nosub\\\",\\\"id\\\":\\\"123\\\"}\"
-- |        [id]
-- |            Int
-- |
-- |    -   added
-- |        \"{\\\"msg\\\":\\\"added\\\",\\\"collection\\\":\\\"users\\\",\\\"id\\\":\\\"5gBGjzg9oHMZb9DpR\\\",\\\"fields\\\":{\\\"emails\\\":[{\\\"address\\\":\\\"frosch03\@frosch03.de\\\",\\\"verified\\\":true}],\\\"username\\\":\\\"frosch03\\\"}}\"
-- |        [collection]
-- |            String
-- |        [id]
-- |            String
-- |        [fields]
-- |            [emails]
-- |                [(String, Bool)]
-- |            [username]
-- |                String
-- |
-- |    -   result
-- |        \"{\\\"msg\\\":\\\"result\\\",\\\"id\\\":\\\"42\\\",\\\"result\\\":{\\\"id\\\":\\\"5gBGjzg9oHMZb9DpR\\\",\\\"token\\\":\\\"HjAw6Kfwr3Co-J2U7I6L6wnoAlSEKAnzWH7lyqkcK7D\\\",\\\"tokenExpires\\\":{\\\"$date\\\":1559406539449},\\\"type\\\":\\\"password\\\"}}\"
-- |        [id]
-- |            Int
-- |        [result]
-- |            [id]
-- |                String
-- |            [token]
-- |                String
-- |            [tokenExpires]
-- |                [date]
-- |                    Int
-- |
-- |            [type]
-- |                Password
-- |
-- "{\"msg\":\"result\",\"id\":\"75392\",\"result\":{\"_id\":\"087851f6c85844dabf972c87bb758c5746b74f1322555babfc69e39ddb003ce0\",\"rid\":\"5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR\",\"msg\":\"75392 [der ORF zieht sich von Facebook zurck.Das sollten ...](http://blog.fefe.de/?ts=a260a82b)\",\"ts\":{\"$date\":1553892993456},\"u\":{\"_id\":\"dNfBQiWGorDmHwWXR\",\"username\":\"lambdabot\",\"name\":\"lambdabot\"},\"_updatedAt\":{\"$date\":1553892993465},\"urls\":[{\"url\":\"http://blog.fefe.de/?ts=a260a82b\"}],\"mentions\":[],\"channels\":[]}}"
-- |    -   changed
-- |        \"{\\\"msg\\\":\\\"changed\\\",\\\"collection\\\":\\\"stream-notify-user\\\",\\\"id\\\":\\\"id\\\",\\\"fields\\\":{\\\"eventName\\\":\\\"5gBGjzg9oHMZb9DpR\/notification\\\",\\\"args\\\":[{\\\"title\\\":\\\"\@admin\\\",\\\"text\\\":\\\"test\\\",\\\"payload\\\":{\\\"_id\\\":\\\"h4c9fvZhvqNLu2bSR\\\",\\\"rid\\\":\\\"5gBGjzg9oHMZb9DpRoH9xE5Zs4mvSByHm3\\\",\\\"sender\\\":{\\\"_id\\\":\\\"oH9xE5Zs4mvSByHm3\\\",\\\"username\\\":\\\"admin\\\",\\\"name\\\":\\\"Administrator\\\"},\\\"type\\\":\\\"d\\\",\\\"message\\\":{\\\"msg\\\":\\\"test\\\"}}}]}}\"
-- |        [collection]
-- |            Stream-notify-user
-- |        [id]
-- |            String – id
-- |        [fields]
-- |            [eventName]
-- |                (String, Notification)
-- |            [args]
-- |                [
-- |                [title]
-- |                    String
-- |                [text]
-- |                    String
-- |                [payload]
-- |                    [id]
-- |                        String
-- |                    [rid]
-- |                        String
-- |                    [sender]
-- |                        [id]
-- |                            String
-- |                        [username]
-- |                            String – admin
-- |                        [name]
-- |                            String – Administrator
-- |
-- |                    [type]
-- |                        Char – d (direct)
-- |                    [message]
-- |                        [msg]
-- |                            String



-- Send Message:
-- {"msg":"method","method":"sendMessage","id":"42","params":[{"_id":"8","rid":"5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR","msg":"Hello"}]}

-- "{\"msg\":\"result\",\"id\":\"42\",\"result\":{\"_id\":\"6\",\"rid\":\"5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR\",\"msg\":\"Hello\",\"ts\":{\"$date\":1552737969353},\"u\":{\"_id\":\"dNfBQiWGorDmHwWXR\",\"username\":\"lambdabot\",\"name\":\"lambdabot\"},\"_updatedAt\":{\"$date\":1552737969370},\"mentions\":[],\"channels\":[]}}"
-- result :: (RF2)
--   _id :: String / Int
--   rid :: String
--   msg :: String
--   ts  ::
--     $date :: Int
--   u   ::
--     _id      :: String
--     username :: String
--     name     :: String
--   _updatedAt ::
--     $date :: Int
--   mentions :: []
--     --
--   channels :: []
--     --

-- "{\"msg\":\"result\",\"id\":\"42\",\"error\":{\"isClientSafe\":true,\"error\":500,\"reason\":\"Internal server error\",\"message\":\"Internal server error [500]\",\"errorType\":\"Meteor.Error\"}}"
-- error :: (ER)
--   isClientSafe :: Bool
--   error        :: Int
--   reason       :: String
--   message      :: String
--   errorType    :: String
--                       | ER  Bool Int String String String

-- "{\"msg\":\"result\",\"id\":\"42\",\"error\":{\"isClientSafe\":true,\"error\":500,\"reason\":\"Internal server error\",\"message\":\"Internal server error [500]\",\"errorType\":\"Meteor.Error\"}}"
