-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.ChangedField.Datatype
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.ChangedField.Datatype
where

import Data.RocketChat.Message.ChangedField.NotificationType
import Data.RocketChat.Message.ChangedField.ChangedFieldArgs

data ChangedField
    = CF
      { cfEventName :: (String, NotificationType)
      , cfArgs      :: [ChangedFieldArgs]
      }
    deriving (Eq, Show)

