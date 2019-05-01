-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.Constructors
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.Constructors
where

import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Method

mkSendMsg :: Int -> RoomId -> String -> Message
mkSendMsg id rid text = Mtd (mkSendMethod id rid text)
              
