module RssPush
    ( initialize
    , sense
    , process
    , actuate
    )
where

import Aux (secondsOfTheDay)
import Ascii (ascii)
import Message (mkSendMsg)
import Method (RoomId)
import Plugin.RSS.Auxiliary (grepFeeds)
import Plugin.RSS.Commands

import FRP.Yampa (DTime, SF, arr, loopPre)
import Data.Text (pack)
import Data.Maybe (catMaybes)
import Network.WebSockets (sendTextData)
import Network.WebSockets.Connection (Connection(..)) 
import Control.Concurrent (threadDelay)

changed :: SF [(Bool, (String, [String]))] [(Bool, (String, [String]))]
changed =
    loopPre [] (arr isNew)

isNew :: Eq a => ([(Bool, a)], [(Bool, a)]) -> ([(Bool, a)], [(Bool, a)])
isNew (xs, ys)
      | (length xs) == (length ys)
      = (xs', xs)

      | (length xs) > (length ys)
      = (xs, xs)

      | otherwise
      = (ys, xs)
    where
      xs' = fn xs ys
      fn  = (\xs ys -> zipWith (\(_, x) (_, y) -> if (x == y)
                                                then (True,  y)
                                                else (False, x)) xs ys)

process :: SF [(Bool, (String, [String]))] [(Bool, (String, [String]))]
process =
    changed

initialize :: Connection -> IO [(Bool, (String, [String]))]
initialize c = do
    datas <- grepData 0
    putStrLn "initialized"
    return $ map (\x -> (False, x)) datas

sense :: Connection -> Bool -> IO (DTime, Maybe [(Bool, (String, [String]))])
sense c _ = do
    datas <- grepData (pushInterval . pushs $ rssConfig)
    return (0.0, Just $ map (\x -> (False, x)) datas)

actuate :: Connection -> Bool -> [(Bool, (String, [String]))] -> IO Bool
actuate c _ datas = do
    mid <- secondsOfTheDay
    let sendToRC = (\r -> sendTextData c . pack . ascii . mkSendMsg mid r)
    sequence . concat $ [ map (flip sendToRC $ msg) rids | (equal, (msg, rids)) <- datas, not equal ]
    return False

sec2µs :: Int -> Int
sec2µs =
    floor . (* 1E6) . fromIntegral

grepData :: Int -> IO [(String, [String])]
grepData delay = do
    threadDelay . sec2µs $ delay
    let (io_feeds, rooms) = unzip [ (return . head =<< grepFeeds feed fns 1, rooms) | (Feed feed fns, rooms) <- pushFeedIntoRoomss .  pushs $ rssConfig ]
    feeds <- sequence io_feeds
    return $ zip feeds rooms