module Method
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Data.List (intercalate)

import AuxParser
import Aux
import Algo
import Ascii  

type MsgId = String
type RoomId = String

data Method
    = Login String String Algo
    | SendMsg [(MsgId, RoomId, String)]
    | Logout
    deriving (Eq)

instance Ascii (Method) where
    ascii (Login usr pwd SHA256) =
        "\"method\":\"login\",\"params\":[{\"user\":{\"username\":" ++ (show usr) ++
        "},\"password\":{\"digest\":" ++ (show . sha256hash $ pwd) ++
        ",\"algorithm\":\"" ++ (ascii SHA256) ++
        "\"}}]"
    ascii (SendMsg msgs) =
        "\"method\":\"sendMessage\",\"params\":[" ++
        (intercalate "," . map fn $ msgs) ++ "]"
            where
              fn (i,r,m) = "{\"_id\":\"" ++ i ++ "\",\"rid\":\"" ++ r ++ "\",\"msg\":\"" ++ m ++ "\"}"

instance Show (Method) where
    show (Login usr pwd _) =
        "Login: " ++ usr ++ "(" ++ hidePw pwd ++ ")"
    show (SendMsg msgs) =
        "SendMsg" ++ (intercalate "; SendMsg" . map fn $ msgs)
            where
              fn (i,r,m) = "(ID:" ++ i ++ ", RID:" ++ r ++ "): " ++ m
    show (Logout) =
        "Logout"

pMethod :: GenParser Char st Method
pMethod = do
      try pLogin
  <|> try pSendMsg
  <|> try pLogout


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


pSendMsgParams :: GenParser Char st [(String, String, String)]
pSendMsgParams = pValOfParams pSendMsgParam


pSendMsg :: GenParser Char st Method
pSendMsg = do
  result <- permute
    (    (\_ _ b -> (b))
    <$$> (try $ pKeyVal ("method", "sendMessage"))
    <||> (char ',')
    <||> (try $ pSendMsgParams)
    )
  return $ SendMsg result

pLogout :: GenParser Char st Method
pLogout = do
  pKeyVal ("method", "logout")
  return Logout

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

pUser :: GenParser Char st String
pUser = do 
  string "\"user\":{"
  usr <- pValOfKey "username"
  char '}'
  return usr
         
pLoginParam :: GenParser Char st (String, (String, Algo))
pLoginParam = do
  char '{'
  result <- permute
    (    (\a _ b -> (a,b))
    <$$> (try $ pUser)
    <||> (char ',')
    <||> (try $ pPassword)
    )
  char '}'
  return result

pLoginParams :: GenParser Char st [(String, (String, Algo))]
pLoginParams = pValOfParams pLoginParam
  -- string "\"params\":["
  -- lp <- sepBy pLoginParam (char ',')
  -- char ']'
  -- return lp

pLogin :: GenParser Char st Method
pLogin = do
  result <- permute
    (    (\_ _ b -> (b))
    <$$> (try $ pKeyVal ("method", "login"))
    <||> (char ',')
    <||> (try $ pLoginParams)
    )
  return $ (\((usr, (pw, algo)):_) -> Login usr pw algo) result
  
  
