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


pSendMsg :: GenParser Char st Method
pSendMsg = do
  (i, smp) <- permute
    (    (\_ _ b _ c -> (b, c))
    <$$> (try $ pKeyVal ("method", "sendMessage"))
    <||> (char ',')
    <||> (try $ pValOfKey "id")
    <||> (char ',')
    <||> (try $ pParams pSendMsgParam)
    )
  return $ SendMsg (read i) smp

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

pUsername :: GenParser Char st String
pUsername = do 
  string "\"user\":{"
  usr <- pValOfKey "username"
  char '}'
  return usr
         
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


pLogin :: GenParser Char st Method
pLogin = do
  (i, lp) <- permute
    (    (\_ _ b _ c -> (b,c))
    <$$> (try $ pKeyVal ("method", "login"))
    <||> (char ',')
    <||> (try $ pValOfKey "id")
    <||> (char ',')
    <||> (try $ pParams pLoginParam)
    )
  return $ (\((usr, (pw, algo)):_) -> Login (read i) usr pw algo) lp
  
  
