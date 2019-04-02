module Plugin.RSS.Commands where

import Plugin.RSS.Reader (readFeed, title, link, description)

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Text.Read (readMaybe)

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

lastNxkcd :: (String -> IO ()) -> String -> IO ()
lastNxkcd fn s = do
  let n = if (length . tail . words $ s) > 0
          then maybe 1 id (readMaybe (head . tail . words $ s) :: Maybe Int)
          else 1
  xs <- readFeed n "https://www.xkcd.com/rss.xml"
  let ts = map title       xs
      ls = map link        xs
      ds = map description xs
      ms = zipWith3 (\d t l -> "[" ++ t ++ "](" ++ l ++ "): " ++ (grepImgUrl d)) ds ts ls
  mapM (\m -> putStrLn m) ms
  mapM fn ms
  return ()


lastNfefe :: (String -> IO ()) -> String -> IO ()
lastNfefe fn s = do
  let n = if (length . tail . words $ s) > 0
          then maybe 1 id (readMaybe (head . tail . words $ s) :: Maybe Int)
          else 1
  xs <- readFeed n "http://blog.fefe.de/rss.xml?html"
  let ts = map title xs
      ls = map link  xs
      ms = zipWith (\t l -> "[" ++ t ++ "](" ++ l ++ ")") ts ls
  mapM (\m -> putStrLn m) ms
  mapM fn ms
  return ()

