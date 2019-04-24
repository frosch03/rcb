module Data.RocketChat.Message.Datatype
where

import Data.RocketChat.Message.AddedField
import Data.RocketChat.Message.ResultField
import Data.RocketChat.Message.Collection
import Data.RocketChat.Message.ChangedField
import Data.RocketChat.Message.Method

data Message
-- Write only part -----------
    = Pong
    | Con
      { conVersion             :: Int
      , conSupport             :: Int
      }
    | Mtd
      { method                 :: Method
      }
    | AddedUserCol
      { addedUserColId         :: String
      , addedUserColUsername   :: String
      }       -- Added Username id
    | SubStreamNotifyUser
      { streamNotifyUserId     :: Int
      , streamNotifyUserUserId :: String
      }   -- SubStreamNotifyUser Id UserId
-- Read only part -----------
    | Error
      { errorReason            :: String
      }
    | Server
      { serverId               :: Int
      }
    | Ping
    | Connected
      { connectedSession       :: String
      }        -- Session: String
    | Ready
      { readySubs              :: [Int]
      }             -- Subs: [Int]
    | Updated
      { updatedMethods         :: [Int]
      }           -- Methods: [Int]
    | Nosub
      { nosubId                :: Int
      }
    | Added
      { addedCollection        :: String
      , addedId                :: String
      , addedField             :: AddedField
      }
    | Result
      { resultId               :: Int
      , resultField            :: ResultField
      }
    | Changed
      { changedCollection      :: Collection
      , changedId              :: String
      , changedField           :: ChangedField
      }
    deriving (Eq)
