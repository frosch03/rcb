-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.Parser
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.Parser
where

import Data.RocketChat.AuxiliaryParsers
import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Method
import Data.RocketChat.Message.ChangedField.Parser
import Data.RocketChat.Message.ResultField.Parser
import Data.RocketChat.Message.AddedField


import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

pMessage :: GenParser Char st Message
pMessage = do
        try pPing
    <|> try pError
    <|> try pServer
    <|> try pConnected
    <|> try pReady
    <|> try pUpdated
    <|> try pNosub
    <|> try pAdded
    <|> try pResult
    <|> try pChanged
    <|> try pMtd

pPing :: GenParser Char st Message
pPing = do
  char '{'
  pKeyVal ("msg", "ping")
  char '}'
  return Ping

pError :: GenParser Char st Message
pError = do
  char '{'
  pKeyVal ("msg", "error") ; char ','
  reason <- pValOfKey "reason"
  char '}'
  return $ Error reason

pServer :: GenParser Char st Message
pServer = do
  char '{'
  id <- pValOfKey "server_id"
  char '}'
  return $ Server (read id)
  
pConnected :: GenParser Char st Message
pConnected = do
  char '{'
  pKeyVal ("msg", "connected") ; char ','
  session <- pValOfKey "session"
  char '}'
  return (Connected session)

pReady :: GenParser Char st Message
pReady = do
  char '{'
  pKeyVal ("msg", "ready") ; char ','
  string $ "\"subs\":["
  xs <- sepBy pQuotedString (char ',')
  string $ "]}"
  return (Ready $ map read xs)

pUpdated :: GenParser Char st Message
pUpdated = do
  char '{'
  pKeyVal ("msg", "updated") ; char ','
  string $ "\"methods\":["
  xs <- sepBy pQuotedString (char ',')
  string $ "]}"
  return (Updated $ map read xs)

pNosub :: GenParser Char st Message
pNosub = do
  char '{'
  pKeyVal ("msg", "nosub") ; char ','
  id <- pValOfKey "id"
  char '}'
  return (Nosub $ read id)

pAdded :: GenParser Char st Message
pAdded = do
  char '{'
  pKeyVal ("msg", "added")            ; char ','
  (collection, id, af) <- permute
         (    (\a _ b _ c -> (a,b,c))
         <$$> (try $ pValOfKey "collection")
         <||> (char ',')
         <||> (try $ pValOfKey "id")
         <||> (char ',')
         <||> (try $ pAddedField)
         )
  return (Added collection id af)

-- "{\"msg\":\"result\",\"id\":\"42\",\"result\":{\"_id\":\"6\",\"rid\":\"5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR\",\"msg\":\"Hello\",\"ts\":{\"$date\":1552737969353},\"u\":{\"_id\":\"dNfBQiWGorDmHwWXR\",\"username\":\"lambdabot\",\"name\":\"lambdabot\"},\"_updatedAt\":{\"$date\":1552737969370},\"mentions\":[],\"channels\":[]}}"

-- data AuthType         = Password
-- data ResultField      = RF String String Int AuthType
-- .. Result Int ResultField
pResult :: GenParser Char st Message
pResult = do 
  char '{'
  pKeyVal ("msg", "result") ; char ','
  (id, rf) <- permute
         (    (\a _ b -> (a, b))
         <$$> (try $ pValOfKey "id")
         <||> (char ',')
         <||> (try pResultField <|> try pResultField2 <|> try pResultField3 <|> try pResultField4 <|> try pResultFieldError)
         )
  char '}'
  return $ Result (read id) rf

pChanged :: GenParser Char st Message
pChanged = do
  char '{'
  pKeyVal ("msg", "changed")           ; char ','
  (col, s, chg) <- permute
         (    (\a _ b _ c -> (a,b,c))
         <$$> (try $ pValOfKey "collection")
         <||> (char ',')
         <||> (try $ pValOfKey "id")
         <||> (char ',')
         <||> (try $ pChangedField)
         )
  char '}'
  return $ Changed (read col) s chg

pMtd :: GenParser Char st Message
pMtd = do
  char '{'
  pKeyVal ("msg", "method") ; char ','
  methd <- pMethod
  char '}'
  return $ Mtd methd
  
pAddedMsg :: GenParser Char st Message
pAddedMsg = do
  char '{'
  pKeyVal ("msg", "added")        ; char ','
  pKeyVal ("collection", "users") ; char ','
  id   <- (pValOfKey "id")         ; char ','
  user <- pUsernameFromFields
  char '}'
  return (AddedUserCol user id)
