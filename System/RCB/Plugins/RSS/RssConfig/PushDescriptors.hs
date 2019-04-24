module System.RCB.Plugins.RSS.RssConfig.PushDescriptors
where

import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor

-- Datatype
data PushDescriptors
    = Push
      { pushFeedIntoRoomss :: [(FeedDescriptor, [Room])]
      , pushInterval       :: Int
      }
    deriving (Show)

-- Sub-Datatype
data Room
    = Room
      { room_name :: String
      , room_id   :: String
      , room_type :: RoomType
      }
    deriving (Show, Eq, Ord)

-- SubSub-Datatype
data RoomType = Direct | Group deriving (Show, Read, Eq, Ord)

