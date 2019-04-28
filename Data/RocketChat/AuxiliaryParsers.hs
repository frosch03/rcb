module Data.RocketChat.AuxiliaryParsers
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

  
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

pPOfKey :: (GenParser Char st a) -> String -> GenParser Char st a
pPOfKey pP key = do
  string $ "\"" ++ key ++ "\":"
  s <- pP
  return $ s

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

pUrl :: GenParser Char st String
pUrl = do
  char '{'
  url <- pValOfKey "url"
  char '}'
  return url

pUrls :: GenParser Char st [String]
pUrls = do
  string "\"urls\":["
  urls <- sepBy pUrl (char ',')
  char ']'
  return urls

pMethodDetailsOfKey :: String -> GenParser Char st String
pMethodDetailsOfKey key = do
  string $ "\"" ++ key ++ "\":{"
  mtd <- pValOfKey "method"
  char '}'
  return $ mtd

pUser :: GenParser Char st (String, String, String)
pUser = do 
  char '{'
  result <- permute
         (    (\a _ b _ c -> (a,b,c))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValOfKey "username")
         <||> (char ',')
         <||> (try $ pValOfKey "name")
         )
  char '}'
  return result

pSender :: GenParser Char st (String, String, String)
pSender = do
  string "\"sender\":"
  result <- pUser
  return result

pInnerMessage :: GenParser Char st String
pInnerMessage = do
  string "\"message\":{"
  msg <- pValOfKey "msg"
  char '}'
  return msg
       

pSendMsgParam :: GenParser Char st (String, String, String)
pSendMsgParam = do
  char '{'
  result <- permute
    (    (\a _ b _ c -> (a,b,c))
    <$$> (try $ pValOfKey "_id")
    <||> (char ',')
    <||> (try $ pValOfKey "rid")
    <||> (char ',')
    <||> (try $ pValOfKey "msg")
    )
  char '}'
  return result 

pUsername :: GenParser Char st String
pUsername = do 
  string "\"user\":{"
  usr <- pValOfKey "username"
  char '}'
  return usr
