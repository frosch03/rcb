-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
where    

import System.RCB.Plugins.RSS.RssConfig.FeedTransformer

-- Datatype
data FeedDescriptor
    = Feed
      { feedUrl         :: String
      , feedTransformer :: FeedTransformer
      }
    deriving (Show, Read)


-- Defaults
defaultFeed :: FeedDescriptor
defaultFeed = Feed "" (FeedTransformer Nothing Nothing (Just Dempty))
