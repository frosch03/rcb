module AuxParser
where

import Text.ParserCombinators.Parsec
  
pBool :: GenParser Char st Bool
pBool
    =     (string "true"  >> return True)
      <|> (string "True"  >> return True)
      <|> (string "false" >> return False)
      <|> (string "False" >> return False)

pBoolOfKey :: String -> GenParser Char st Bool
pBoolOfKey key = do
  string $ "\"" ++ key ++ "\":"
  s <- pBool
  return (s)

pIntOfKey :: String -> GenParser Char st Int
pIntOfKey key = do
  string $ "\"" ++ key ++ "\":"
  i <- many digit
  return (read i)

pQuotedString :: GenParser Char st String
pQuotedString
    = do char '\"'
         xs  <- many1 $ noneOf "\""
         char '\"'
         return $ xs
           
pInt :: GenParser Char st Int
pInt
    = do try ( do (char '-')
                  ds <- many digit
                  return (-1 * (read ds))
             )
         <|> ( do ds <- many digit
                  return ((read ds))
             )

pValOfKey :: String -> GenParser Char st String
pValOfKey key = do
  string $ "\"" ++ key ++ "\":"
  s <- pQuotedString
  return (s)


pParams :: (GenParser Char st a) -> GenParser Char st [a]
pParams parser = do
  string "\"params\":["
  xs <- sepBy parser (char ',')
  char ']'
  return xs

pKeyVal :: (String, String) -> GenParser Char st ()
pKeyVal (key, val) = do
  string $ "\"" ++ key ++ "\":"
  string $ "\"" ++ val ++ "\""
  return ()

pUsernameFromFields :: GenParser Char st String
pUsernameFromFields = do
  string $ "\"fields\":"      ; char '{'
  user <- pValOfKey "username" ; char ','
  string "\"emails\":" 
  many1 (noneOf "]")          ; char ']'
  char '}'
  return user


-- instance Read (Bool) where
--     readsPrec p s = case parse pBool "" s of
--                       Left s  -> error $ "error while parsing Bool" ++ (show s)
--                       Right x -> [(x, "")]
