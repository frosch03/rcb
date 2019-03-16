module Algo
where
   
import Text.ParserCombinators.Parsec

import AuxParser
import Ascii

data Algo
    = SHA256
    deriving (Eq)

instance Ascii (Algo) where
    ascii SHA256 = "sha-256"

instance Show (Algo) where
    show SHA256 = "SHA256"

  
pAlgo :: GenParser Char st Algo
pAlgo = do
  string "sha-256"
  return SHA256


instance Read (Algo) where
    readsPrec p s = case parse pAlgo "" s of
                      Left s  -> error $ "error while parsing Algo / " ++ (show s)
                      Right x -> [(x, "")]
         
