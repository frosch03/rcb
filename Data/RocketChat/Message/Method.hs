module Data.RocketChat.Message.Method
where

import System.RCB.Auxiliary
import Data.RocketChat.AuxiliaryParsers
import Data.RocketChat.Message.Algo

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Data.List (intercalate)

type MsgId = String
type RoomId = String

-- Constructors
mkSendMethod :: Int -> RoomId -> String -> Method
mkSendMethod id rid text =
    SendMsg id [(mid, rid, text)]
    where
      mid = sha256hash $ text ++ show id

-- Datatype
data Method
    = Login Int String String Algo
    | SendMsg Int [(MsgId, RoomId, String)]
    | Logout
    deriving (Eq)

-- Instances
instance Show (Method) where
    show (Login id usr pwd _) =
        "(id:" ++ show id ++ ") Login " ++ usr ++ "(" ++ hidePw pwd ++ ")"
    show (SendMsg id msgs) =
        "(id:" ++ show id ++ ") SendMsg " ++ (intercalate "; SendMsg" . map fn $ msgs)
            where
              fn (i,r,m) = "(ID:" ++ i ++ ", RID:" ++ r ++ "): " ++ m
    show (Logout) =
        "Logout"

-- Parser
pMethod :: GenParser Char st Method
pMethod = do
      try pLogin
  <|> try pSendMsg
  <|> try pLogout


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

-- Writer
