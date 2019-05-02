-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Instances
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Instances
where

import System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Datatype
import System.RCB.Plugins.RSS.ITransformable

import Text.ParserCombinators.Parsec


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
                                          
