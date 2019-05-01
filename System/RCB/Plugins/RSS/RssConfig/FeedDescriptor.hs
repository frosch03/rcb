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

import Text.ParserCombinators.Parsec

-- Datatype
data FeedDescriptor
    = Feed
      { feedUrl         :: String
      , feedTransformer :: FeedTransformer
      }

-- Instances
instance Show (FeedDescriptor) where
    show (Feed url _) = url

-- Defaults
defaultFeed :: FeedDescriptor
defaultFeed = Feed "" (FeedTransformer Nothing Nothing (Just Dempty))
  
data TTitle
    = Tempty
    | Tdrop_quotes

data TLink
    = Lempty

data TDescription
    = Dempty
    | Dimgurl

class Transformable a where
      tfunc :: a -> String -> String
      lfunc :: a -> String -> String
      dfunc :: a -> String -> String


data FeedTransformer
    = FeedTransformer
      { feedTransformer_title :: Maybe TTitle
      , feedTransformer_link :: Maybe TLink
      , feedTransformer_description :: Maybe TDescription
      }

instance Transformable FeedTransformer where
    tfunc (FeedTransformer (Just Tempty) _ _) = const ""
    tfunc (FeedTransformer (Just Tdrop_quotes) _ _) = filter (not . flip elem ['`', '"'])
    tfunc (FeedTransformer Nothing _ _) = id

    lfunc (FeedTransformer _ (Just Lempty) _) = const ""
    lfunc (FeedTransformer _ Nothing _) = id

    dfunc (FeedTransformer _ _ (Just Dempty)) = const ""
    dfunc (FeedTransformer _ _ (Just Dimgurl)) = grepImgUrl_local
    dfunc (FeedTransformer _ _ Nothing) = id


grepImgUrl_local :: String -> String
grepImgUrl_local s = fst $
    case (parse pImageTag' "" s) of
      Left err  -> error $ show err
      Right xs  -> xs
    where
      pImageTag' :: GenParser Char st (String, String)
      pImageTag' = do
        many $ noneOf "<"
        string "<img src=\""
        src <- manyTill anyChar (try $ string "\" title")
        string "=\""
        title <- manyTill anyChar (try $ string "\" alt")
        string "=\""
        alt <- manyTill anyChar (try $ string "\" />")
        many anyChar
        return (src, title)
