-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Auxiliary
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Auxiliary
where

import Data.Char (ord)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS (unpack, pack)
import Text.Printf (printf)
import Data.Time.Clock (diffTimeToPicoseconds, utctDayTime, getCurrentTime)
import Text.Read (readMaybe)
import FRP.Yampa (SF, arr, loopPre)
import System.IO


secondsOfTheDay :: IO Int
secondsOfTheDay = getCurrentTime >>= (\x -> return $ fromInteger . (flip div $ 1000000000000) . diffTimeToPicoseconds . utctDayTime $ x)

enclose :: String -> Char -> String
enclose s c = (c:s) ++ [c]
    
hidePw :: String -> String
hidePw = map (\_ -> '*') 
        
sha256hash :: String -> String
sha256hash = concatMap (printf "%02x") . BS.unpack . hash . BS.pack . map (fromIntegral.ord)
  
countFromMsg :: String -> Maybe Int
countFromMsg s =
    if (length . tail . words $ s) > 0
    then readMaybe . head . tail . words $ s
    else Nothing
  

changed :: SF [(Bool, (String, [String]))] [(Bool, (String, [String]))]
changed =
    loopPre [] (arr isNew)

isNew :: Eq a => ([(Bool, a)], [(Bool, a)]) -> ([(Bool, a)], [(Bool, a)])
isNew (xs, ys)
      | (length xs) == (length ys)
      = (xs', xs)

      | (length xs) > (length ys)
      = (xs, xs)

      | otherwise
      = (ys, xs)
    where
      xs' = fn xs ys
      fn  = (\xs ys -> zipWith (\(_, x) (_, y) -> if (x == y)
                                                then (True,  y)
                                                else (False, x)) xs ys)

sec2µs :: Int -> Int
sec2µs =
    floor . (* 1E6) . fromIntegral


store :: Show a => String -> a -> IO ()
store filename cfg = do
  putStrLn $ "Storing into: " ++ filename
  handle <- openFile filename WriteMode
  hPutStr handle . show $ cfg
  hClose handle

