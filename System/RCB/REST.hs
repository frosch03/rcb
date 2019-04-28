module System.RCB.REST
where

import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.RssConfig.Modifiers
import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import Data.RocketChat.AuxiliaryParsers
    
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import Data.CaseInsensitive
import Data.Aeson
-- import Control.Applicative
import Control.Concurrent.MVar

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

    
domain :: String
domain = "c.frosch03.de"

userId, authTk :: String
-- userId = "Uncomment and add sensible contetnt"
-- authTk = "Uncomment and add sensible contetnt"

userId = "dNfBQiWGorDmHwWXR"
authTk = "mRwPwVYV1e6TNLT-ym4w6NJQs70u_ZpmkGWXHw4YRHL"

data Presence = Offline | Online
              deriving (Show, Read)

data UsersInfoUser
    = UsersInfoUser
      { usersinfo_id       :: String
      , usersinfo_username :: String
      , usersinfo_type     :: String -- UserType
      , usersinfo_status   :: String -- Presence
      , usersinfo_active   :: Bool
      , usersinfo_name     :: String
      }
    | UsersInfoError
      { usersinfo_error     :: String
      , usersinfo_errortype :: String
      }
    deriving (Eq, Show)

instance Read (UsersInfoUser) where
    readsPrec p s = case parse pUsersInfoResponse "" s of
                      Left s  -> error $ "error while parsing UsersInfo-User / " ++ (show s)
                      Right (_, x) -> [(x, "")]


pUsersInfoResponse :: GenParser Char st (Bool, UsersInfoUser)
pUsersInfoResponse = do
  char '{'
  (suc, uiu) <- permute
         (    (\a _ b -> (a, b))
         <$$> (try $ pBoolOfKey ("success"))
         <||> (char ',')
         <||> (try $ pUsersInfoUser)
         )
  char '}'
  return (suc, uiu)


pUsersInfoUser :: GenParser Char st UsersInfoUser
pUsersInfoUser = do
        try pUsersInfoUser'
    <|> try pUsersInfoUser''
    <|> try pUsersInfoError


pUsersInfoUser' :: GenParser Char st UsersInfoUser
pUsersInfoUser' = do
  string "\"user\":{"
  (id, u, utype, stat, act, name) <- permute
         (    (\a _ b _ c _ d _ e _ f _ _-> (a,b,c,d,e,f))
         <$$> (try $ pValOfKey ("_id"))
         <||> (char ',')
         <||> (try $ pValOfKey "username")
         <||> (char ',')
         <||> (try $ pValOfKey "type")
         <||> (char ',')
         <||> (try $ pValOfKey "status")
         <||> (char ',')
         <||> (try $ pBoolOfKey "active")
         <||> (char ',')
         <||> (try $ pValOfKey "name")
         <||> (char ',')
         <||> (try $ pIntOfKey "utcOffset")
         )
  char '}'
  return (UsersInfoUser id u utype stat act name)

pUsersInfoUser'' :: GenParser Char st UsersInfoUser
pUsersInfoUser'' = do
  string "\"user\":{"
  (id, u, utype, stat, act, name) <- permute
         (    (\a _ b _ c _ d _ e _ f -> (a,b,c,d,e,f))
         <$$> (try $ pValOfKey ("_id"))
         <||> (char ',')
         <||> (try $ pValOfKey "username")
         <||> (char ',')
         <||> (try $ pValOfKey "type")
         <||> (char ',')
         <||> (try $ pValOfKey "status")
         <||> (char ',')
         <||> (try $ pBoolOfKey "active")
         <||> (char ',')
         <||> (try $ pValOfKey "name")
         )
  char '}'
  return (UsersInfoUser id u utype stat act name)

pUsersInfoError :: GenParser Char st UsersInfoUser
pUsersInfoError = do
  (err, errtype) <- permute
         (    (\a _ b -> (a, b))
         <$$> (try $ pValOfKey "error")
         <||> (char ',')
         <||> (try $ pValOfKey "errorType")
         )
  return (UsersInfoError err errtype)


x :: String -> Request
x s =
    defaultRequest
    { method = BSC8.pack "GET"
    , host = BSC8.pack $ domain
    , path = BSC8.pack $ "/api/v1/" ++ s
    , requestHeaders =
        [ (mk $ BSC8.pack "X-User-Id",    BSC8.pack userId)
        , (mk $ BSC8.pack "X-Auth-Token", BSC8.pack authTk)
        ]
    }

test s = newManager tlsManagerSettings >>= \m -> httpLbs (x s) m


fillRoomIDs :: MVar RssConfig -> IO ()
fillRoomIDs config = do
  cfg <- takeMVar config
  let rs = Prelude.map room_name $ allRoomsUniq cfg
  rids <- mapM (directMessageRoomId "lambdabot") rs
  let rtois = Prelude.zip rs rids
      newcfg = updateRooms cfg rtois
  putMVar config newcfg

directMessageRoomId :: String -> String -> IO (String)
directMessageRoomId fromUser toUser = do
  fromUser_ <- getUser fromUser
  toUser_   <- getUser toUser
  return . Prelude.foldl1 (++) . Prelude.map usersinfo_id $ [toUser_, fromUser_]
  

getUser :: String -> IO UsersInfoUser
getUser s = do
  r <- test $ "users.info?username=" ++ s
  let r' = (read . LBSC8.unpack . responseBody $ r) :: UsersInfoUser
  return r'


data RestUser
    = RestUser
      { restUser_id :: String
      , restUser_username :: String
      , restUser_name :: String
      }
    deriving (Show)

instance Read (RestUser) where
    readsPrec p s = case parse pRestUser "" s of
                      Left s  -> error $ "error while parsing RestUser / " ++ (show s)
                      Right x -> [(x, "")]

data RestMessage
    = RestMessage
      { restMessage_id :: String
      , restMessage_rid :: String
      , restMessage_msg :: String
      , restMessage_ts :: String
      , restMessage_u :: RestUser
      , restMessage_updatedAt :: String
      , restMessage_editedBy :: Maybe String
      , restMessage_editedAt :: Maybe String
      , restMessage_emoji :: Maybe String
      , restMessage_avatar :: Maybe String
      , restMessage_alias :: Maybe String
      , restMessage_customFields :: Maybe [String]
      , restMessage_groupable :: Maybe String
      , restMessage_attachments :: Maybe [String]
      , restMessage_reactions :: Maybe [String]
      , restMessage_mentions :: [String]
      , restMessage_channels :: [String]
      , restMessage_sandstormSessionId :: Maybe String
      }
    | RestMessageWURL
      { restMessage_id :: String
      , restMessage_rid :: String
      , restMessage_msg :: String
      , restMessage_ts :: String
      , restMessage_u :: RestUser
      , restMessage_updatedAt :: String
      , restMessage_editedBy :: Maybe String
      , restMessage_editedAt :: Maybe String
      , restMessage_emoji :: Maybe String
      , restMessage_avatar :: Maybe String
      , restMessage_alias :: Maybe String
      , restMessage_customFields :: Maybe [String]
      , restMessage_groupable :: Maybe String
      , restMessage_attachments :: Maybe [String]
      , restMessage_reactions :: Maybe [String]
      , restMessage_urls :: [String]
      , restMessage_mentions :: [String]
      , restMessage_channels :: [String]
      , restMessage_sandstormSessionId :: Maybe String
      }
    deriving (Show)

instance Read (RestMessage) where
    readsPrec p s = case parse pRestMessage "" s of
                      Left s  -> error $ "error while parsing RestMessage / " ++ (show s)
                      Right x -> [(x, "")]

data ImList
    = ImList
      { imList_id :: String
      , imList_usernames :: [String]
      , imList_updatedAt :: String
      , imList_t :: String
      , imList_msgs :: Int
      , imList_ts :: String
      , imList_usercount :: Int
      , imList_lm :: String
      , imList_lastMessage :: RestMessage
      }
    | ImListNoLast
      { imList_id :: String
      , imList_usernames :: [String]
      , imList_updatedAt :: String
      , imList_t :: String
      , imList_msgs :: Int
      , imList_ts :: String
      , imList_usercount :: Int
      }
    deriving (Show)

instance Read (ImList) where
    readsPrec p s = case parse pRestImList "" s of
                      Left s  -> error $ "error while parsing Rest ImList / " ++ (show s)
                      Right x -> [(x, "")]

data RestResponse
    = RestResponse
      { restResponse_ims :: [ImList]
      , restResponse_offset :: Int
      , restResponse_count :: Int 
      , restResponse_total :: Int 
      , restResponse_success :: Bool
      }
    deriving (Show)
    
instance Read (RestResponse) where
    readsPrec p s = case parse pRestResponse "" s of
                      Left s  -> error $ "error while parsing Rest Response / " ++ (show s)
                      Right x -> [(x, "")]


pRestImLists :: GenParser Char st [ImList]
pRestImLists = do
  char '['
  ims <- (sepBy pRestImList (char ','))
  char ']'
  return ims

pRestResponse :: GenParser Char st RestResponse
pRestResponse = do 
  char '{'
  (ims, off, count, tot, succ) <- permute
         (    (\a _ b _ c _ d _ e -> (a,b,c,d,e))
         <$$> (try $ pPOfKey pRestImLists "ims")
         <||> (char ',')
         <||> (try $ pIntOfKey "offset")
         <||> (char ',')
         <||> (try $ pIntOfKey "count")
         <||> (char ',')
         <||> (try $ pIntOfKey "total")
         <||> (char ',')
         <||> (try $ pBoolOfKey "success")
         )
  char '}'
  return $ RestResponse ims off count tot succ


pMaybeStringOfKey :: String -> GenParser Char st (Maybe String)
pMaybeStringOfKey = pPOfKey pP
    where
      pP = do   (try $ string "null" >> return Nothing)
            <|> (pQuotedString >>= (return . Just))

pMaybeStringsOfKey :: String -> GenParser Char st (Maybe [String])
pMaybeStringsOfKey = pPOfKey pP
    where
      parser = do
        char '['
        xs <- (sepBy pQuotedString (char ','))
        char ']'
        return . Just $ xs
      pP = do   (try $ string "null" >> return Nothing)
            <|> parser

pKeyedURLs :: String -> String -> GenParser Char st [String]
pKeyedURLs key = pPOfKey pP
  where
    pP = do   (try $ string "null" >> return [])
           <|> outerParser
    outerParser = do
      char '['
      xs <- (sepBy innerParser (char ','))
      char ']'
      return xs

    innerParser = do
      char '{'
      string $ "\"" ++ key ++ "\":"
      s <- pQuotedString
      char '}'
      return s

pMaybeURLsOfKey :: String -> GenParser Char st (Maybe [String])
pMaybeURLsOfKey = pPOfKey pP
    where
      parser = do
        char '['
        xs <- (sepBy (pQuotedString) (char ','))
        char ']'
        return . Just $ xs
      pP = do   (try $ string "null" >> return Nothing)
            <|> parser

pValsOfKey :: String -> GenParser Char st [String]
pValsOfKey = pPOfKey parser
    where
      parser = do
        char '['
        xs <- (sepBy pQuotedString (char ','))
        char ']'
        return xs

pRestUser :: GenParser Char st RestUser
pRestUser = do 
  char '{'
  (id, username, name) <- permute
         (    (\a _ b _ c -> (a,b,c))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValOfKey "username")
         <||> (char ',')
         <||> (try $ pValOfKey "name")
         )
  char '}'
  return $ RestUser id username name

pRestImList :: GenParser Char st ImList
pRestImList = do
      (try pRestImList')
  <|> (try pRestImListNoLM)

pRestImList' :: GenParser Char st ImList
pRestImList' = do
  char '{'
  (id, uname, updatedAt, t, msgs, ts, ucount, lm, lastMsg) <- permute
         (    (\a _ b _ c _ d _ e _ f _ g _ h _ i -> (a,b,c,d,e,f,g,h,i))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValsOfKey "usernames")
         <||> (char ',')
         <||> (try $ pValOfKey "_updatedAt")
         <||> (char ',')
         <||> (try $ pValOfKey "t")
         <||> (char ',')
         <||> (try $ pIntOfKey "msgs")
         <||> (char ',')
         <||> (try $ pValOfKey "ts")
         <||> (char ',')
         <||> (try $ pIntOfKey "usersCount")
         <||> (char ',')
         <||> (try $ pValOfKey "lm")
         <||> (char ',')
         <||> (try $ pPOfKey pRestMessage "lastMessage")
         )
  char '}'
  return $ ImList id uname updatedAt t msgs ts ucount lm lastMsg
  
pRestImListNoLM :: GenParser Char st ImList
pRestImListNoLM = do
  char '{'
  (id, uname, updatedAt, t, msgs, ts, ucount) <- permute
         (    (\a _ b _ c _ d _ e _ f _ g -> (a,b,c,d,e,f,g))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValsOfKey "usernames")
         <||> (char ',')
         <||> (try $ pValOfKey "_updatedAt")
         <||> (char ',')
         <||> (try $ pValOfKey "t")
         <||> (char ',')
         <||> (try $ pIntOfKey "msgs")
         <||> (char ',')
         <||> (try $ pValOfKey "ts")
         <||> (char ',')
         <||> (try $ pIntOfKey "usersCount")
         )
  char '}'
  return $ ImListNoLast id uname updatedAt t msgs ts ucount
  


pRestMessage :: GenParser Char st RestMessage
pRestMessage = do
      (try pRestMessage')
  <|> (try pRestMessageWURL)

pRestMessageWURL :: GenParser Char st RestMessage
pRestMessageWURL = do 
  char '{'
  (id, rid, msg, ts, usr, updatedAt, editBy, editAt, emo, ava, ali, custom, groupbl, attach, react, urls, ments, chans, sandstrm) <- permute
         (    (\a _ b _ c _ d _ e _ f _ g _ h _ i _ j _ k _ l _ m _ n _ o _ p _ q _ r _ s -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValOfKey "rid")
         <||> (char ',')
         <||> (try $ pValOfKey "msg")
         <||> (char ',')
         <||> (try $ pValOfKey "ts")
         <||> (char ',')
         <||> (try $ pPOfKey pRestUser "u")
         <||> (char ',')
         <||> (try $ pValOfKey "_updatedAt")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "editedBy")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "editedAt")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "emoji")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "avatar")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "alias")
         <||> (char ',')
         <||> (try $ pMaybeStringsOfKey "customFields")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "groupable")
         <||> (char ',')
         <||> (try $ pMaybeStringsOfKey "attachments")
         <||> (char ',')
         <||> (try $ pMaybeStringsOfKey "reactions")
         <||> (char ',')
         <||> (try $ (pKeyedURLs "url") "urls")
         <||> (char ',')
         <||> (try $ pValsOfKey "mentions")
         <||> (char ',')
         <||> (try $ pValsOfKey "channels")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "sandstormSessionId")
         )
  char '}'
  return $ RestMessageWURL id rid msg ts usr updatedAt editBy editAt emo ava ali custom groupbl attach react urls ments chans sandstrm

pRestMessage' :: GenParser Char st RestMessage
pRestMessage' = do 
  char '{'
  (id, rid, msg, ts, usr, updatedAt, editBy, editAt, emo, ava, ali, custom, groupbl, attach, react, ments, chans, sandstrm) <- permute
         (    (\a _ b _ c _ d _ e _ f _ g _ h _ i _ j _ k _ l _ m _ n _ o _ p _ q _ r -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValOfKey "rid")
         <||> (char ',')
         <||> (try $ pValOfKey "msg")
         <||> (char ',')
         <||> (try $ pValOfKey "ts")
         <||> (char ',')
         <||> (try $ pPOfKey pRestUser "u")
         <||> (char ',')
         <||> (try $ pValOfKey "_updatedAt")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "editedBy")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "editedAt")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "emoji")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "avatar")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "alias")
         <||> (char ',')
         <||> (try $ pMaybeStringsOfKey "customFields")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "groupable")
         <||> (char ',')
         <||> (try $ pMaybeStringsOfKey "attachments")
         <||> (char ',')
         <||> (try $ pMaybeStringsOfKey "reactions")
         <||> (char ',')
         <||> (try $ pValsOfKey "mentions")
         <||> (char ',')
         <||> (try $ pValsOfKey "channels")
         <||> (char ',')
         <||> (try $ pMaybeStringOfKey "sandstormSessionId")
         )
  char '}'
  return $ RestMessage id rid msg ts usr updatedAt editBy editAt emo ava ali custom groupbl attach react ments chans sandstrm
