-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.ResultField.Parser
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.ResultField.Parser
where

import Data.RocketChat.Message.ResultField.Datatype
import Data.RocketChat.AuxiliaryParsers

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

-- "{\"msg\":\"result\",\"id\":\"42\",\"result\":{\"_id\":\"6\",\"rid\":\"5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR\",\"msg\":\"Hello\",\"ts\":{\"$date\":1552737969353},\"u\":{\"_id\":\"dNfBQiWGorDmHwWXR\",\"username\":\"lambdabot\",\"name\":\"lambdabot\"},\"_updatedAt\":{\"$date\":1552737969370},\"mentions\":[],\"channels\":[]}}"
-- result :: (RF2)
--   _id :: String / Int
--   rid :: String
--   msg :: String
--   ts  ::
--     $date :: Int
--   u   ::
--     _id      :: String
--     username :: String
--     name     :: String
--   _updatedAt ::
--     $date :: Int
--   mentions :: []
--     --
--   channels :: []
--     --
pDollarDate :: String -> GenParser Char st Int
pDollarDate name = do
  string $ "\"" ++ name ++ "\":{\"$date\":"
  date <- many digit
  char '}'
  return $ read date

pTokenExpires :: GenParser Char st Int
pTokenExpires = do
  string "\"tokenExpires\":{\"$date\":"
  -- date <- many1 $ noneOf "}" 
  date <- many digit
  char '}'
  return $ read date

pResultField2 :: GenParser Char st ResultField
pResultField2 = do
  string "\"result\":{"
  (id1, rid, msg, date1, u, date2) <- permute
    (    (\a _ b _ c _ d _ e _ f _ _ _ _ -> (a,b,c,d,e,f))
    <$$> (try $ pValOfKey "_id")
    <||> (char ',') 
    <||> (try $ pValOfKey "rid") 
    <||> (char ',') 
    <||> (try $ pValOfKey "msg")
    <||> (char ',') 
    <||> (try $ pDollarDate "ts")
    <||> (char ',') 
    <||> (try $ string "\"u\":" >> pUser)
    <||> (char ',') 
    <||> (try $ pDollarDate "_updatedAt")
    <||> (char ',') 
    <||> (try $ string "\"mentions\":[]")
    <||> (char ',') 
    <||> (try $ string "\"channels\":[]")
    )
  char '}'
  return $ RF2 id1 rid msg date1 u date2

pResultField3 :: GenParser Char st ResultField
pResultField3 = do
  string "\"result\":{"
  (id1, rid, msg, date1, u, date2, urls) <- permute
    (    (\a _ b _ c _ d _ e _ f _ g _ _ _ _ -> (a,b,c,d,e,f,g))
    <$$> (try $ pValOfKey "_id")
    <||> (char ',') 
    <||> (try $ pValOfKey "rid") 
    <||> (char ',') 
    <||> (try $ pValOfKey "msg")
    <||> (char ',') 
    <||> (try $ pDollarDate "ts")
    <||> (char ',') 
    <||> (try $ string "\"u\":" >> pUser)
    <||> (char ',') 
    <||> (try $ pDollarDate "_updatedAt")
    <||> (char ',') 
    <||> (try $ pUrls)
    <||> (char ',') 
    <||> (try $ string "\"mentions\":[]")
    <||> (char ',') 
    <||> (try $ string "\"channels\":[]")
    )
  char '}'
  return $ RF3 id1 rid msg date1 u date2 urls [] []


-- "{\"msg\":\"result\",\"id\":\"81216\",\"error\":{\"isClientSafe\":true,\"error\":\"error-invalid-room\",\"reason\":\"Invalid room\",\"details\":{\"method\":\"canAccessRoom\"},\"message\":\"Invalid room [error-invalid-room]\",\"errorType\":\"Meteor.Error\"}}"
-- error :: (ER2)
--   isClientSafe :: Bool
--   error        :: String
--   reason       :: String
--   details      :: Method :: String
--   message      :: String
--   errorType    :: String

-- "{\"msg\":\"result\",\"id\":\"42\",\"error\":{\"isClientSafe\":true,\"error\":500,\"reason\":\"Internal server error\",\"message\":\"Internal server error [500]\",\"errorType\":\"Meteor.Error\"}}"
-- error :: (ER)
--   isClientSafe :: Bool
--   error        :: Int
--   reason       :: String
--   message      :: String
--   errorType    :: String

pResultFieldError :: GenParser Char st ResultField
pResultFieldError = do
      try pResultFieldError1
  <|> try pResultFieldError2

pResultFieldError1 :: GenParser Char st ResultField
pResultFieldError1 = do
  string "\"error\":{"
  (safe, erno, reason, msg, typ) <- permute
    (    (\a _ b _ c _ d _ e -> (a,b,c,d,e))
    <$$> (try $ pBoolOfKey "isClientSafe")
    <||> (char ',')
    <||> (try $ pIntOfKey "error")
    <||> (char ',')
    <||> (try $ pValOfKey "reason")
    <||> (char ',')
    <||> (try $ pValOfKey "message")
    <||> (char ',')
    <||> (try $ pValOfKey "errorType")
    )
  char '}'
  return $ ER1 safe erno reason msg typ
--         ER Bool Int String String String

pResultFieldError2 :: GenParser Char st ResultField
pResultFieldError2 = do
  string "\"error\":{"
  (safe, err, reason, dets, msg, typ) <- permute
    (    (\a _ b _ c _ d _ e _ f -> (a,b,c,d,e,f))
    <$$> (try $ pBoolOfKey "isClientSafe")
    <||> (char ',')
    <||> (try $ pValOfKey "error")
    <||> (char ',')
    <||> (try $ pValOfKey "reason")
    <||> (char ',')
    <||> (try $ pMethodDetailsOfKey "details")
    <||> (char ',')
    <||> (try $ pValOfKey "message")
    <||> (char ',')
    <||> (try $ pValOfKey "errorType")
    )
  char '}'
  return $ ER2 safe err reason dets msg typ


pResultField :: GenParser Char st ResultField
pResultField = do
  string "\"result\":{"
  (i, tk, tke, tp) <- permute
         (    (\a _ b _ c _ d -> (a,b,c,d))
         <$$> (try $ pValOfKey "id")
         <||> (char ',')
         <||> (try $ pValOfKey "token")
         <||> (char ',')
         <||> (try $ pTokenExpires)
         <||> (char ',')
         <||> (try $ pValOfKey "type")
         )
  char '}'
  return $ RF i tk tke (read tp)
