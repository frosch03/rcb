module System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Instances
where

import System.RCB.Plugins.RSS.RssConfig.FeedTransformer.Datatype
import System.RCB.Plugins.RSS.ITransformable

import Data.Monoid
import Data.Semigroup
import Text.ParserCombinators.Parsec


instance Transformable TTitle where
    transform Tempty = const ""
    transform Tdrop_quotes = filter (not . flip elem ['`', '"'])

instance Transformable TLink where
    transform Lempty = const ""

instance Transformable TDescription where
    transform Dempty = const ""
    transform Dimgurl = grepImgUrl

instance Transformable a => Transformable (Maybe a) where
    transform (Just x) = transform x
    transform Nothing  = id

instance Semigroup TTitle where
    (<>) = const

instance Semigroup TLink where
    (<>) = const

instance Semigroup TDescription where
    (<>) = const

instance Semigroup FeedTransformer where
    (<>) (FeedTransformer fTx fLx fDx) (FeedTransformer fTy fLy fDy) =
        (FeedTransformer (mappend fTx fTy) (mappend fLx fLy) (mappend fDx fDy))

instance Monoid TTitle where
    mempty = Tempty

instance Monoid TLink where
    mempty = Lempty

instance Monoid TDescription where
    mempty = Dempty

instance Monoid FeedTransformer where
    mempty = FeedTransformer Nothing Nothing Nothing



grepImgUrl :: String -> String
grepImgUrl s = fst $
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
                                          
