module Data.RocketChat.Message.Algo
where

import Data.RocketChat.AuxiliaryParsers

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

-- Constructors
-- Datatype
data Algo
    = SHA256
    deriving (Eq)

-- Instances
instance Show (Algo) where
    show SHA256 = "SHA256"

instance Read (Algo) where
    readsPrec p s = case parse pAlgo "" s of
                      Left s  -> error $ "error while parsing Algo / " ++ (show s)
                      Right x -> [(x, "")]

-- Parser
pAlgo :: GenParser Char st Algo
pAlgo = do
  string "sha-256"
  return SHA256

pPassword :: GenParser Char st (String, Algo)
pPassword = do
  string "\"password\":{"
  (pw, algo) <- permute
    (    (\a _ b -> (a,b))
    <$$> (try $ pValOfKey "digest")
    <||> (char ',')
    <||> (try $ pValOfKey "algorithm")
    )
  char '}'
  return (pw, read algo)
         
pLoginParam :: GenParser Char st (String, (String, Algo))
pLoginParam = do
  char '{'
  result <- permute
    (    (\a _ b -> (a,b))
    <$$> (try $ pUsername)
    <||> (char ',')
    <||> (try $ pPassword)
    )
  char '}'
  return result

-- Writer
  
