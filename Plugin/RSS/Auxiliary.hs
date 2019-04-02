module Plugin.RSS.Auxiliary where

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

grepNfeed :: Int -> String -> ((String -> String), (String -> String), (String -> String)) -> (String -> IO ()) -> String -> IO ()
grepNfeed i feed (tFn, lFn, dFn) postFn s = do
  let n = if (length . tail . words $ s) > 0
          then maybe i id (readMaybe (head . tail . words $ s) :: Maybe Int)
          else i
  xs <- readFeed n feed
  let ts = map title       xs
      ls = map link        xs
      ds = map description xs
      ms = zipWith3 (\d t l -> "[" ++ (tFn t) ++ "](" ++ (lFn l) ++ "): " ++ (dFn d)) ds ts ls
  mapM putStrLn ms
  mapM postFn ms
  return ()
    
