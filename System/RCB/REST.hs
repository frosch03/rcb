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

