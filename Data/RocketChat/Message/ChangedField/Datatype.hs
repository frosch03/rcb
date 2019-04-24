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

