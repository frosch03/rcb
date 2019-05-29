-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.REST.Modifiers
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.REST.Modifiers
where

import System.RCB.Room
import System.RCB.Plugins.REST.JiraConfig

import Data.List (group, sort)

-- To avoid a circle with the following line
-- import System.RCB.Plugins.RSS.Auxiliary
-- here is just the implementation
uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = map head . group . sort

-- Cata-Morphisms
allJiraRooms :: JiraConfig -> [Room]
allJiraRooms config = concat . map snd $ pushList
    where
      pushList = jiraPushs config

allJiraRoomsUniq :: JiraConfig -> [Room]
allJiraRoomsUniq = uniq . allJiraRooms
