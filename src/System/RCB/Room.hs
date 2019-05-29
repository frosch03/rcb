-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Room
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Room
where

data Room
    = Room
      { room_name :: String
      , room_id   :: String
      , room_type :: RoomType
      }
    deriving (Show, Read, Eq, Ord)

-- Sub-Datatype
data RoomType = Direct | Group deriving (Show, Read, Eq, Ord)

