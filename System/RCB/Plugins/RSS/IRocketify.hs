module System.RCB.Plugins.RSS.IRocketify
where

import System.RCB.Plugins.RSS.RssConfig.Datatype
import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
import System.RCB.Plugins.RSS.RssConfig.PushDescriptors

import Data.List (groupBy, group, sort)

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
