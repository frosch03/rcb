{-# LANGUAGE OverloadedStrings #-}

module RCT where
 
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive
import Data.Aeson
import Control.Applicative

domain :: String
domain = "c.frosch03.de"

userId, authTk :: String
userId = ""
authTk = ""

x :: String -> Request
x s =
    defaultRequest
    { method = pack "GET"
    , host = pack $ domain
    , path = pack $ "/api/v1/" ++ s
    , requestHeaders =
        [ (mk $ pack "X-User-Id",    pack userId)
        , (mk $ pack "X-Auth-Token", pack authTk)
        ]
    }

test s = newManager tlsManagerSettings >>= \m -> httpLbs (x s) m

