-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Datatype
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Datatype
where

data TTitle
    = Tempty
    | Tdrop_quotes
    deriving (Show, Read, Eq)

data TLink
    = Lempty
    deriving (Show, Read, Eq)

data TDescription
    = Dempty
    | Dimgurl
    deriving (Show, Read, Eq)

data FeedTransformer
    = FeedTransformer
      { feedTransformer_title :: Maybe TTitle
      , feedTransformer_link :: Maybe TLink
      , feedTransformer_description :: Maybe TDescription
      }
    deriving (Show, Read, Eq)
