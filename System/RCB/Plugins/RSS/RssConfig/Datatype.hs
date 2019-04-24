module System.RCB.Plugins.RSS.RssConfig.Datatype
where

import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor

data RssConfig
    = RssConfig
      { feeds :: [(String, FeedDescriptor)]
      , pushs :: PushDescriptors
      }
    deriving (Show)

