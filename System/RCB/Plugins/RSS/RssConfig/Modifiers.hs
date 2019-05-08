-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.RssConfig.Modifiers
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.RssConfig.Modifiers
where

import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
import System.RCB.Plugins.RSS.RssConfig.FeedTransformer

import Data.List (groupBy, group, sort)
import Data.Maybe (listToMaybe, maybe)
    
-- To avoid a circle with the following line
-- import System.RCB.Plugins.RSS.Auxiliary
-- here is just the implementation
uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = map head . group . sort

-- Homo-Morphisms
addFeed :: RssConfig -> (String, FeedDescriptor) -> RssConfig
addFeed oldCfg inp =
    oldCfg { feeds = (inp:(feeds oldCfg)) }

addRssCommand :: RssConfig -> (String, String) -> RssConfig
addRssCommand config (keyword, url) =
    config { feeds = command:(feeds config) }
    where
      command = (keyword, defaultFeed { feedUrl = url })

addPushToRoom_ :: RssConfig -> FeedTransformer -> (Room, String) -> RssConfig
addPushToRoom_ config ftr (room, url) =
    maybe (config { pushs = Push ((defaultFeed { feedUrl = url, feedTransformer = ftr }, [room]) : pushList) timeout })
          (\i -> config { pushs = Push (   (take (i - 1) pushList)
                                       ++ [ (f, room:rooms) | (f@(Feed inner_url _), rooms) <- pushList, inner_url == url ]
                                       ++ (drop i pushList))
                                      timeout })
          position
    where
      (Push pushList timeout) = (pushs config)
      position = listToMaybe [ x | (x, (Feed inner_url _, _)) <- zip [1..] pushList, inner_url == url]

addPushToRoom :: RssConfig -> (Room, String) -> RssConfig
addPushToRoom config = addPushToRoom_ config (feedTransformer defaultFeed)


delRssCommand :: RssConfig -> Int -> RssConfig
delRssCommand config i =
    config { feeds = take (i - 1) (feeds config) ++ drop i (feeds config) }

delPushToRoom :: RssConfig -> Int -> RssConfig
delPushToRoom config i =
    config { pushs = Push pushs' timeout }
    where
      (Push pushList timeout) = pushs config
      pushs_cntexp = (concat [ map ((,) (url, fns)) rooms | (Feed url fns, rooms) <- pushList ])
      pushs_cegrp  = groupBy (\((x, _), _) ((y, _), _) -> x == y) pushs_cntexp
      pushs_cegrp' = (take (i - 1) pushs_cegrp ++ drop i pushs_cegrp)
      pushs' =  map (foldl (\(_, brooms) (a, aroom) -> (uncurry Feed a, (aroom:brooms))) (Feed "" (FeedTransformer Nothing Nothing Nothing), []))
                    pushs_cegrp'

updateRooms :: RssConfig -> [(String, String)] -> RssConfig
updateRooms config rtoid = 
    config { pushs = Push pushs' timeout }
    where
      (Push pushList timeout) = pushs config
      pushs_cntexp = (concat [ map ((,) (url, fns)) rooms | (Feed url fns, rooms) <- pushList ])
      fn cur@(c1_, (Room cname cid ct)) news
          | length cid == 0 && (length . filter ((== cname) . fst) $ rtoid) > 0
          = (c1_, Room cname (snd . head . filter ((== cname) . fst) $ rtoid) ct):news

          | otherwise
          = cur:news
      pushs_ceupd = foldr fn [] pushs_cntexp
      pushs_cegrp  = groupBy (\((x, _), _) ((y, _), _) -> x == y) pushs_ceupd
      pushs' =  map (foldl (\(_, brooms) (a, aroom) -> (uncurry Feed a, (aroom:brooms))) (Feed "" (FeedTransformer Nothing Nothing Nothing), []))
                    pushs_cegrp


-- Cata-Morphisms 
allRooms :: RssConfig -> [Room]
allRooms config = concat . map snd $ pushList
    where 
      (Push pushList timeout) = pushs config

allRoomsUniq :: RssConfig -> [Room]
allRoomsUniq = uniq . allRooms
