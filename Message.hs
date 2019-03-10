module Message
where

import Text.ParserCombinators.Parsec

import Ascii
import Method
import AuxParser

-- - Messages
--   - connected \\
--     "{\"msg\":\"connected\",\"session\":\"RGKknraTEmHoaE4MP\"}"
--     - session :: String
--   - ready \\
--     "{\"msg\":\"ready\",\"subs\":[\"23\"]}"
--     - subs :: [Int]
--   - updated \\
--     "{\"msg\":\"updated\",\"methods\":[\"42\"]}"
--     - methods :: [Int]
--   - nosub \\
--     "{\"msg\":\"nosub\",\"id\":\"123\"}"
--     - id :: Int
--   - added \\
--     "{\"msg\":\"added\",\"collection\":\"users\",\"id\":\"5gBGjzg9oHMZb9DpR\",\"fields\":{\"emails\":[{\"address\":\"frosch03@frosch03.de\",\"verified\":true}],\"username\":\"frosch03\"}}"
--     - collection :: String
--     - id :: String
--     - fields ::
--       - emails :: [(String, Bool)]
--       - username :: String
--   - result \\
--     "{\"msg\":\"result\",\"id\":\"42\",\"result\":{\"id\":\"5gBGjzg9oHMZb9DpR\",\"token\":\"HjAw6Kfwr3Co-J2U7I6L6wnoAlSEKAnzWH7lyqkcK7D\",\"tokenExpires\":{\"$date\":1559406539449},\"type\":\"password\"}}"
--     - id :: Int
--     - result ::
--       - id :: String
--       - token :: String
--       - tokenExpires ::
--         - date :: Int
--       - type :: Password
--   - changed \\
--     "{\"msg\":\"changed\",\"collection\":\"stream-notify-user\",\"id\":\"id\",\"fields\":{\"eventName\":\"5gBGjzg9oHMZb9DpR/notification\",\"args\":[{\"title\":\"@admin\",\"text\":\"test\",\"payload\":{\"_id\":\"h4c9fvZhvqNLu2bSR\",\"rid\":\"5gBGjzg9oHMZb9DpRoH9xE5Zs4mvSByHm3\",\"sender\":{\"_id\":\"oH9xE5Zs4mvSByHm3\",\"username\":\"admin\",\"name\":\"Administrator\"},\"type\":\"d\",\"message\":{\"msg\":\"test\"}}}]}}"
--     - collection :: Stream-notify-user
--     - id :: String -- id
--     - fields ::
--       - eventName :: (String, Notification)
--       - args :: [
--         - title :: String
--         - text :: String
--         - payload :: 
--           - _id :: String
--           - rid :: String
--           - sender ::
--             - _id :: String
--             - username :: String -- admin
--             - name :: String -- Administrator
--           - type :: Char -- d (direct)
--           - message ::
--             - msg :: String
    



--               Email, Valid
type Emails = [(String, Bool)]

data AuthType         = Password                      deriving (Eq, Show)
data ResultField      = RF String String Int AuthType deriving (Eq, Show)
data Collection       = StreamNotifyUser              deriving (Eq, Show)
data NotificationType = Notification                  deriving (Eq, Show)
data ChangedFieldArgs = CFA String String (String, String, (String, String, String), Char, String) deriving (Eq, Show)
data ChangedField     = CF (String, NotificationType) [ChangedFieldArgs] deriving (Eq, Show)
data AddedField       = AF Emails String    deriving (Eq, Show)

data Message
-- Write only part -----------
      = Pong
      | Con Int Int
      | Mtd Method
      | AddedUserCol String String       -- Added Username id
      | SubStreamNotifyUser Int String   -- SubStreamNotifyUser Id UserId
-- Read only part -----------
      | Ping
      | Connected String        -- Session: String
      | Ready [Int]             -- Subs: [Int]
      | Updated [Int]           -- Methods: [Int]
      | Nosub Int
      | Added String String AddedField
      | Result Int ResultField
      | Changed Collection String ChangedField
      deriving (Eq)
              
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
    show (Ping) =
        "PING"
    show (Pong) =
        "PONG"
    show (Con v s) =
        "Connect (version: " ++ show v ++ ", support: " ++ show s ++ ")"
    show (Mtd m) =
        show m
    show (SubStreamNotifyUser id user) =
        "SUBSCRIBE USER NOTOFICATIONS (id: " ++ show id ++ ", userid: " ++ user ++")"
-- --------------
    show (Connected s) =
        "CONNECTED (Session:" ++ s ++ ")"
    show (Ready subs) =
        "READY (Subs:" ++ show subs ++ ")"
    show (Updated methods) =
        "UPDATED (Methods:" ++ show methods ++ ")"
    show (Nosub i) =
        "UNSUBSCRIBE (id: " ++ show i ++ ")"
    show (Added collection id fields) =
        "ADDED (Collection: " ++ collection ++ ", Id: " ++ id ++ ", " ++ show fields ++ ")"
    show (Result id result) =
        "RESULT (Id: " ++ (show id) ++ ", " ++ (show result) ++ ")"
    show (Changed collection id cf) =
        "CHANGED: " ++ (show collection) ++ " (ID: " ++ id ++ ", " ++ show cf ++")"
  

pMessage :: GenParser Char st Message
pMessage =
        pPing
    <|> pConnected
    <|> pReady
    <|> pUpdated
    <|> pNosub
    <|> pAdded
    <|> pResult
    <|> pChanged

instance Read (Message) where
    readsPrec p s = case parse pMessage "" s of
                      Left _  -> error $ "error while parsing Message"
                      Right x -> [(x, "")]

pAuthType :: GenParser Char st AuthType
pAuthType
    =     (string "password" >> return Password)
      <|> (string "Password" >> return Password)

instance Read (AuthType) where
    readsPrec p s = case parse pAuthType "" s of
                      Left _  -> error $ "error while parsing AuthType"
                      Right x -> [(x, "")]


pPing :: GenParser Char st Message
pPing = do
  char '{'
  pKeyVal ("msg", "ping")
  char '}'
  return Ping
         

pCollection :: GenParser Char st Collection
pCollection
    =     (string "stream-notify-user" >> return StreamNotifyUser)
      <|> (string "Stream-Notify-User" >> return StreamNotifyUser)

instance Read (Collection) where
    readsPrec p s = case parse pCollection "" s of
                      Left _  -> error $ "error while parsing Collection"
                      Right x -> [(x, "")]


pNotificationType :: GenParser Char st NotificationType
pNotificationType =
        (string "notification" >> return Notification)
    <|> (string "Notification" >> return Notification)

instance Read (NotificationType) where
    readsPrec p s = case parse pNotificationType "" s of
                      Left _  -> error $ "error while parsing NotificationType"
                      Right x -> [(x, "")]

pConnected :: GenParser Char st Message
pConnected = do
  char '{'
  pKeyVal ("msg", "connected") >> char ','
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

--  | Added String String AddedField
-- data AddedField       = AF [(String, Bool)] String

pEmail :: GenParser Char st (String, Bool)
pEmail = do
  char '{'
  email <- pValOfKey "address" ; char ','
  string "\"verified\":"
  verified <- pBool
  char '}'
  return (email, verified)

pAdded :: GenParser Char st Message
pAdded = do
  char '{'
  pKeyVal ("msg", "added")            ; char ','
  collection <- pValOfKey "collection" ; char ','
  id         <- pValOfKey "id"         ; char ','
  string "\"fields\":{"
  string "\"emails\":["
  emails <- sepBy pEmail (char ',')
  char ']'                            ; char ','
  username <- pValOfKey "username"
  string "}}"
  return (Added collection id (AF emails username))

-- data AuthType         = Password
-- data ResultField      = RF String String Int AuthType
-- .. Result Int ResultField
pResult :: GenParser Char st Message
pResult = do 
  char '{'
  pKeyVal ("msg", "result") ; char ','
  id <- pValOfKey "id"       ; char ','
  string "\"result\":{"
  rid   <- pValOfKey "id"    ; char ','
  token <- pValOfKey "token" ; char ','
  string "\"tokenExpires\":{\"$date\":"
  date <- many1 $ noneOf "}" 
  char '}'                  ; char ','
  ttype <- pValOfKey "type" 
  char '}'
  char '}'
  return (Result (read id) (RF rid token (read date) (read ttype)))

pChanged :: GenParser Char st Message
pChanged = do -- error "TBD: Method to be defined"
  char '{'
  pKeyVal ("msg", "changed")           ; char ','
  collection <- pValOfKey "collection"  ; char ','
  id         <- pValOfKey "id"          ; char ','
  string "\"fields\":{"
  string "\"eventName\":\""
  userId <- many1 $ noneOf "/"          ; char '/'
  notifyType <- many1 $ noneOf "\""     ; string "\","
  string "\"args\":[{"
  title <- pValOfKey "title"            ; char ','
  text  <- pValOfKey "text"             ; char ','
  string "\"payload\":{"
  id2 <- pValOfKey "_id"                ; char ','
  rid <- pValOfKey "rid"                ; char ','
  string "\"sender\":{"
  id3 <- pValOfKey "_id"                ; char ','
  username <- pValOfKey "username"      ; char ','
  name <- pValOfKey "name"              ; string "},"
  mtype <- pValOfKey "type"             ; char ','
  string "\"message\":{"
  msg <- pValOfKey "msg"
  string "}}}]}}"
  return $ Changed (read collection) id (CF (userId, read notifyType) [(CFA title text (id2, rid, (id3, username, name), (head mtype), msg))])

pAddedMsg :: GenParser Char st Message
pAddedMsg = do
  char '{'
  pKeyVal ("msg", "added")        ; char ','
  pKeyVal ("collection", "users") ; char ','
  id   <- (pValOfKey "id")         ; char ','
  user <- pUsernameFromFields
  char '}'
  return (AddedUserCol user id)
