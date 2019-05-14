-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.REST.JiraConfig
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.REST.JiraConfig where

import System.RCB.Room

data JiraConfig
    = JiraConfig
      { jiraPushs :: [(String, [Room])]
      , jiraInterval :: Int
      }
    deriving (Show, Read, Eq)

