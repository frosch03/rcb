module System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
where    

-- Datatype
data FeedDescriptor
    = Feed
      { feedUrl         :: String
      , feedTransformer :: (String -> String, String -> String, String -> String)
      }

-- Instances
instance Show (FeedDescriptor) where
    show (Feed url _) = url

-- Defaults
defaultFeed :: FeedDescriptor
defaultFeed = Feed "" (id, id, const "")          
  
