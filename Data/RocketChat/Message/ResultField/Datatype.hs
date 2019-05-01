-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.ResultField.Datatype
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.ResultField.Datatype
where

import Data.RocketChat.Message.ResultField.AuthType

data ResultField
    = RF
      { rfId           :: String
      , rfToken        :: String
      , rfTokenExpires :: Int
      , rfType         :: AuthType
      }
    | RF2
      { rf2_Id        :: String
      , rf2rId        :: String
      , rf2msg        :: String
      , rf2Ts         :: (Int)
      , rf2U          :: (String, String, String)
      , rf2_updatedAt :: (Int)
      } -- [a] [a]
    | RF3
      { rf3_Id        :: String
      , rf3rId        :: String
      , rf3msg        :: String
      , rf3Ts         :: (Int)
      , rf3U          :: (String, String, String)
      , rf3_updatedAt :: (Int)
      , rf3_urls      :: [String]
      , rf3_mentions  :: [String]
      , rf3_channels  :: [String]
      }
    | ER1
      { er1IsClientSafe :: Bool
      , er1Error        :: Int
      , er1Reason       :: String
      , er1Message      :: String
      , er1ErrorType    :: String
      }
    | ER2
      { er2IsClientSafe :: Bool
      , er2Error        :: String
      , er2Reason       :: String
      , er2Details      :: String
      , er2Message      :: String
      , er2ErrorType    :: String
      }
    deriving (Eq, Show)
