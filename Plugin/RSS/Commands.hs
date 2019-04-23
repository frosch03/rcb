module Plugin.RSS.Commands where

import Plugin.RSS.Auxiliary

import Data.Maybe (listToMaybe, maybe)    
import Data.List (groupBy, group, sort)

-- type RssConfig = [FeedDescriptor]

data RssConfig
    = RssConfig
      { feeds :: [(String, FeedDescriptor)]
      , pushs :: PushDescriptors
      }
    deriving (Show)

addFeed :: RssConfig -> (String, FeedDescriptor) -> RssConfig
addFeed oldCfg inp =
    oldCfg { feeds = (inp:(feeds oldCfg)) }

data RoomType = Direct | Group deriving (Show, Read, Eq, Ord)

data Room
    = Room
      { room_name :: String
      , room_id   :: String
      , room_type :: RoomType
      }
    deriving (Show, Eq, Ord)

data FeedDescriptor
    = Feed
      { feedUrl         :: String
      , feedTransformer :: (String -> String, String -> String, String -> String)
      }

instance Show (FeedDescriptor) where
    show (Feed url _) = url

data PushDescriptors
    = Push
      { pushFeedIntoRoomss :: [(FeedDescriptor, [Room])]
      , pushInterval       :: Int
      }
    deriving (Show)

class Rocketify a where
    rctify :: a -> String

instance Rocketify RssConfig where
    rctify (RssConfig fds@(fd_:fds_) (Push pss@(ps_:pss_) timeout)) =
        foldl (++) "" $
                 [ "```"
                 , "\\n"
                 , "Feed Commands\\n"
                 , "=============\\n"
                 , "\\n"
                 , "usage: <command> [amount]\\n"
                 , "  amount - number of entries to be retrieved\\n"
                 , "\\n"
                 , fds_header
                 , fds_hline
                 ]
              ++ fds_string
              ++ [ "\\n\\n"
                 , "Push Feeds\\n"
                 , "==========\\n"
                 , "\\n"
                 , "timeout: " ++ show timeout ++ "s\\n"
                 , "\\n"
                 , pss_header
                 , pss_phline
                 ]
              ++ pss_string
              ++ [ "```" ]
            where
              max_fcmd  = foldl (\b (a, _) -> max b $ length a) 0 fds
              max_furl  = foldl (\b (_, (Feed a _)) -> max b $ length a) 0 fds
              max_fnum  = length . show . length $ fds
              max_proom = foldl (\b (_, rs) -> max b $ (maximum . map (length . room_name) $ rs)) 0 pss
              max_purl  = foldl (\b ((Feed a _), _) -> max b $ length a) 0 pss
              max_pnum  = length . show . length $ pss
              pss_cntexp = zip [1..] (concat [ map ((,) (url, fns)) rooms | (Feed url fns, rooms) <- pss ])
              pss_cegrp  = groupBy (\(_, ((x, _), _)) (_, ((y, _), _)) -> x == y) pss_cntexp
              fds_cntexp = [ (show nr, cmd, url) | (nr, (cmd, Feed url _)) <- zip [1..] fds ]
              toString mN mP1 (nr, par1, par2) =    nr   ++ ((mN -  (length nr))   `replicate` ' ') ++ " "
                                                 ++ par1 ++ ((mP1 - (length par1)) `replicate` ' ') ++ " -> "
                                                 ++ par2                                            ++ "\\n"
              toString3 mN mP1 mP2 (nr, par1, par2, par3) =    nr   ++ ((mN -  (length nr))   `replicate` ' ') ++ " "
                                                            ++ par1 ++ ((mP1 - (length par1)) `replicate` ' ') ++ " -> "
                                                            ++ par2 ++ ((mP2 - (length par2)) `replicate` ' ') ++ " "
                                                            ++ par3                                            ++ "\\n"
              withRoom (x,y) (Room u r _)
                  | length r == 0 = (x, y, u, "[ ]")
                  | otherwise    = (x, y, u, "[X]")
              fds_string = map (toString max_fnum max_fcmd) fds_cntexp
              fds_header = toString max_fnum max_fcmd ("#", "Command", "Feed URL")
              fds_hline  = (++ "\\n") $ (max_fnum + 1 + max_fcmd + 4 + max_furl) `replicate` '-'
              pss_header = toString3 max_pnum max_purl max_proom ("#", "URL", "Rooms", "ID")
              pss_phline = (++ "\\n") $ (max_pnum + 1 + max_purl + 4 + max_proom + 1 + 3) `replicate` '-'
              pss_string = concat $ map (\(x:xs) ->
                                             (     ((toString3 max_pnum max_purl max_proom) . (\(nr, ((url, _), room)) -> withRoom (show nr, url) room)) x
                                             : map ((toString3 max_pnum max_purl max_proom) . (\(nr, ((_,   _), room)) -> withRoom (show nr, "")  room)) xs
                                             )
                                        ) pss_cegrp
          

defaultFeed :: FeedDescriptor
defaultFeed = Feed "" (id, id, const "")

addRssCommand :: RssConfig -> (String, String) -> RssConfig
addRssCommand config (keyword, url) =
    config { feeds = command:(feeds config) }
    where
      command = (keyword, defaultFeed { feedUrl = url })

addPushToRoom :: RssConfig -> (Room, String) -> RssConfig
addPushToRoom config (room, url) =
    maybe (config { pushs = Push ((defaultFeed { feedUrl = url }, [room]) : pushList) timeout })
          (\i -> config { pushs = Push (   (take (i - 1) pushList)
                                       ++ [ (f, room:rooms) | (f@(Feed inner_url _), rooms) <- pushList, inner_url == url ]
                                       ++ (drop i pushList))
                                      timeout })
          position
    where
      (Push pushList timeout) = (pushs config)
      position = listToMaybe [ x | (x, (Feed inner_url _, _)) <- zip [1..] pushList, inner_url == url]


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
      pushs' =  map (foldl (\(_, brooms) (a, aroom) -> (uncurry Feed a, (aroom:brooms))) (Feed "" (id, id, id), []))
                    pushs_cegrp'


allRooms :: RssConfig -> [Room]
allRooms config = concat . map snd $ pushList
    where 
      (Push pushList timeout) = pushs config

uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = map head . group . sort

allRoomsUniq :: RssConfig -> [Room]
allRoomsUniq = uniq . allRooms

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
      pushs' =  map (foldl (\(_, brooms) (a, aroom) -> (uncurry Feed a, (aroom:brooms))) (Feed "" (id, id, id), []))
                    pushs_cegrp


                   
rssConfig :: RssConfig
rssConfig =
    RssConfig
    { feeds =
          [ ("xkcd", Feed "https://www.xkcd.com/rss.xml" (id, id, grepImgUrl))
          , ("fefe", Feed "https://blog.fefe.de/rss.xml?html" (filter (not . flip elem ['`', '"']), id, const ""))
          , ("heise", Feed "http://www.heise.de/newsticker/heise.rdf" (id, id, const ""))
          , ("golem", Feed "https://rss.golem.de/rss.php?feed=RSS2.0" (id, id, const ""))
          , ("hackernews", Feed "https://news.ycombinator.com/rss" (id, id, const ""))
          ]

    , pushs =
          Push
            [ (Feed "https://news.ycombinator.com/rss" (id, id, const ""), [Room "frosch03" "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" Direct])
            , (Feed "https://www.xkcd.com/rss.xml" (id, id, grepImgUrl), [Room "frosch03" "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" Direct])
            , (Feed "https://blog.fefe.de/rss.xml?html" (filter (not . flip elem ['`', '"']), id, const ""), [Room "frosch03" "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" Direct])
            ]
            (6 * 60)
    }
