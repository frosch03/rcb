module Plugin.RSS.Auxiliary where

import Plugin.RSS.Reader (readFeed, rss2string)

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

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

grepFeeds :: String -> ((String -> String), (String -> String), (String -> String)) -> Int -> IO [String]
grepFeeds feed fns i = do
  xs <- readFeed i feed
  let ms = map (rss2string fns) xs
  return ms
