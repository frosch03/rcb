module Aux
where

import Data.Char (ord)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS (unpack, pack)
import Text.Printf (printf)
import Data.Time.Clock (diffTimeToPicoseconds, utctDayTime, getCurrentTime)


secondsOfTheDay :: IO Int
secondsOfTheDay = getCurrentTime >>= (\x -> return $ fromInteger . (flip div $ 1000000000000) . diffTimeToPicoseconds . utctDayTime $ x)

enclose :: String -> Char -> String
enclose s c = (c:s) ++ [c]
    
hidePw :: String -> String
hidePw = map (\_ -> '*') 
        
sha256hash :: String -> String
sha256hash = concatMap (printf "%02x") . BS.unpack . hash . BS.pack . map (fromIntegral.ord)
  
