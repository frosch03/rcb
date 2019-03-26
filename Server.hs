module Server
where

import Text.ParserCombinators.Parsec

import AuxParser
    
data Server
   = ServerId Int

instance Show (Server) where
    show (ServerId i) = "SERVERID: " ++ show i

instance Read (Server) where
    readsPrec p s = case parse pServerId "" s of
                      (Left  _) -> error $ "error while parsing Server ID"
                      (Right x) -> [(x, "")]

pServerId :: GenParser Char st Server
pServerId = do
  char '{'
  id <- pValOfKey "server_id"
  char '}'
  return (ServerId $ read id)
