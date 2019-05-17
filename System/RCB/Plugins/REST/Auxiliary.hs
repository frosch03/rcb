-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.REST.Auxiliary
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.REST.Auxiliary where

import System.RCB.Room
import System.RCB.REST
import System.RCB.IRocketify
import System.RCB.Auxiliary
import System.RCB.Configuration
import System.RCB.Plugins.REST.JiraConfig
import System.RCB.Plugins.REST.Configuration (jira_username, jira_password, jira_host)

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent       (forkIO, threadDelay)
import Control.Concurrent.MVar
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Data.CaseInsensitive (mk)
import Text.Regex.PCRE
import Text.JSON
import Data.Maybe
import Data.List
import System.IO

postPOST :: String -> Request
postPOST s =
    defaultRequest
    { method = BSC8.pack "POST"
    , host = BSC8.pack $ jira_host -- domain
    , secure = False
    , path = BSC8.pack $ "/rest" ++ s
    , requestHeaders =
        [ (mk $ BSC8.pack "content-type", BSC8.pack "application/json")
        ]
    , requestBody =
        (RequestBodyBS $ BSC8.pack $ "{ \"username\": \"" ++ jira_username ++ "\","
                                     ++"\"password\": \"" ++ jira_password ++ "\" }")
    }

getGET :: CookieJar -> String -> Request
getGET cj s =
    defaultRequest
    { method = BSC8.pack "GET"
    , host = BSC8.pack $ jira_host -- domain
    , secure = False
    , path = BSC8.pack $ "/rest" ++ s
    , cookieJar = Just cj
    }

getByKey :: JSValue -> String -> Maybe JSValue
getByKey (JSObject obj) key = lookup key (fromJSObject obj)
getByKey _ _ = Nothing

sdlFromIssue :: JSValue -> (String, String, String)
sdlFromIssue jsv = (summary, "http://" ++ jira_host ++ "/browse/" ++ key, description)
    where
      mKey    = getByKey jsv "key"
      mFields = getByKey jsv "fields"
      mSummary     = maybe Nothing ((flip getByKey) "summary")     mFields
      mDescription = maybe Nothing ((flip getByKey) "description") mFields
      rKey         = (maybe (Error "ERROR: Can't find Key") readJSON mKey) :: Result String
      rSummary     = (maybe (Error "ERROR: Can't find Summary") readJSON mSummary ):: Result String
      rDescription = (maybe (Error "ERROR: Can't find Description") readJSON mDescription ):: Result String
      key         = either (const "") id $ resultToEither rKey
      summary     = either (const "-") id $ resultToEither rSummary
      description = either (const "-") id $ resultToEither rDescription

getIssue :: String -> IO (String, String, String)
getIssue s = do
  mgr <- newManager tlsManagerSettings
  ckj <- getJSESSIONID
  res <- httpLbs (getGET ckj ("/api/latest/issue/" ++ s)) mgr
  let rsp = responseBody res
      (Ok jsv) = decode (BSC8.unpack . LBS.toStrict $ rsp) :: Result JSValue
      mFields = getByKey jsv "fields"
      mSummary     = maybe Nothing ((flip getByKey) "summary") mFields
      mDescription = maybe Nothing ((flip getByKey) "description") mFields
      rSummary     = maybe (Error "ERROR: Can't find Summary")     readJSON mSummary :: Result String
      rDescription = maybe (Error "ERROR: Can't find Description") readJSON mDescription :: Result String
      summary     = either id id $ resultToEither rSummary
      description = either id id $ resultToEither rDescription
  return (summary, "http://" ++ jira_host ++ "/browse/" ++ s, description)

searchJql :: String -> IO [(String, String, String)]
searchJql s = do
  mgr <- newManager tlsManagerSettings
  ckj <- getJSESSIONID
  res <- httpLbs (getGET ckj ("/api/latest/search/?jql=" ++ s)) mgr
  let rsp      = responseBody res
      (Ok jsv) = decode (BSC8.unpack . LBS.toStrict $ rsp) :: Result JSValue
      issues   = maybe [] (\(JSArray x) -> x) $ getByKey jsv "issues"
  return $ map sdlFromIssue issues


getJSESSIONID :: IO CookieJar
getJSESSIONID = do
  mgr <- newManager tlsManagerSettings
  res <- httpLbs (postPOST "/auth/latest/session") mgr
  return . responseCookieJar $ res
           




-- | Transform the RssItem into a string that follows the rocket chat
-- format
jira2string :: (String, String, String) -> String
jira2string (ts, ls, ds) =
    "[" ++ (ts) ++ "](" ++ (ls) ++ "): " ++ (ds)


grepJira :: String -> IO [String]
grepJira query = do
  xs <- searchJql query
  let ms = map jira2string xs
  return ms

grepJiraData :: JiraConfig -> Int -> IO [(String, [String])]
grepJiraData cfg delay = do
    threadDelay . sec2µs $ delay
    let (io_queries, rooms) =
            unzip [ (return . head =<< grepJira query, map room_id rooms)
                        | (query, rooms) <- jiraPushs $ cfg ]
    outputs <- sequence io_queries
    return $ zip outputs rooms


jiraStore :: JiraConfig -> IO ()
jiraStore cfg = do
  writeFile rct_jira_config_file . show $ cfg

jiraRestore :: MVar JiraConfig -> IO ()
jiraRestore cfg = do
  mvconf <- takeMVar cfg
  handle <- openFile rct_jira_config_file ReadMode
  line <- hGetLine handle
  hClose handle
  putMVar cfg . read $ line
           
restoreJira :: MVar JiraConfig -> IO ()
restoreJira jiraconfig = do
  putStrLn $ "Restoring: jira config" 
  mvconf <- takeMVar jiraconfig
  handle <- openFile rct_jira_config_file ReadMode
  line <- hGetLine handle
  hClose handle
  putMVar jiraconfig . read $ line


updateJiraCli :: (String -> IO ()) -> MVar JiraConfig -> IO ()
updateJiraCli notify config = do
  forkIO $ do
    fillJiraRoomIDs config
    mvconf <- readMVar config
    store rct_jira_config_file mvconf
    notify . rctify $ mvconf
  return ()


