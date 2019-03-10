module Message
where

import Text.ParserCombinators.Parsec

import Ascii
import Method
import AuxParser

data Message
    = Pong
      | Con Int Int
      | Mtd Method
      | Connected String
      | AddedUserCol String String       -- Added Username id
      | SubStreamNotifyUser Int String   -- SubStreamNotifyUser Id UserId
      | Nosub Int
              
instance Ascii (Message) where
    ascii (Pong) =
        "{\"msg\": \"pong\"}"
    ascii (Con ver sup) =
        "{\"msg\": \"connect\", \"version\":\"" ++ (show ver) ++
        "\",\"support\": [\"" ++ (show sup) ++ "\"]}"
    ascii (Mtd mtd) =
        "{\"msg\": \"method\",\"id\":\"23\"," ++ (ascii mtd) ++ "}"
    ascii (Nosub id) =
        "{\"msg\":\"nosub\",\"id\":\"" ++ show id ++ "\"}"
    ascii (SubStreamNotifyUser id user) =
        "{\"msg\": \"sub\",\"id\": \"" ++ show id ++
        "\",\"name\": \"stream-notify-user\",\"params\":[\"" ++ user ++ "/notification\",false]}"

instance Show (Message) where
    show (Pong) =
        "PONG"
    show (Con v s) =
        "Connect (version: " ++ show v ++ ", support: " ++ show s ++ ")"
    show (Mtd m) =
        show m
    show (Connected s) =
        "CONNECTED (id:" ++ s ++ ")"
    show (SubStreamNotifyUser id user) =
        "SUBSCRIBE USER NOTOFICATIONS (id: " ++ show id ++ ", userid: " ++ user ++")"
    show (Nosub i) =
        "UNSUBSCRIBE (id: " ++ show i ++ ")"
  
-- instance Read (Message) where
--     readsPrec p s = case parse pTBD "" s of
--                       Left _  -> error $ "error while parsing Message"
--                       Right x -> [(x, "")]

pConnected :: GenParser Char st Message
pConnected = do
  char '{'
  pKeyVal ("msg", "connected") >> char ','
  session <- pValOfKey "session"
  char '}'
  return (Connected session)

pAddedMsg :: GenParser Char st Message
pAddedMsg = do
  char '{'
  pKeyVal ("msg", "added")        ; char ','
  pKeyVal ("collection", "users") ; char ','
  id   <- (pValOfKey "id")         ; char ','
  user <- pUsernameFromFields
  char '}'
  return (AddedUserCol user id)

pNosub :: GenParser Char st Message
pNosub = do
  char '{'
  pKeyVal ("msg", "nosub") ; char ','
  id <- pValOfKey "id"
  char '}'
  return (Nosub $ read id)
