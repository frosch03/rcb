module Data.RocketChat.Message.Constructors
where

import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Method

mkSendMsg :: Int -> RoomId -> String -> Message
mkSendMsg id rid text = Mtd (mkSendMethod id rid text)
              
