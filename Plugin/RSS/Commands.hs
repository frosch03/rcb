module Plugin.RSS.Commands where

import Plugin.RSS.Auxiliary

lastNxkcd :: (String -> IO ()) -> String -> IO ()
lastNxkcd = grepNfeed 1 "https://www.xkcd.com/rss.xml" (id, id, grepImgUrl)

lastNfefe :: (String -> IO ()) -> String -> IO ()
lastNfefe = grepNfeed 1 "https://blog.fefe.de/rss.xml?html" (filter (not . flip elem ['`', '"']), id, const "")

lastNheise :: (String -> IO ()) -> String -> IO ()
lastNheise = grepNfeed 1 "http://www.heise.de/newsticker/heise.rdf" (id, id, const "")

lastNgolem :: (String -> IO ()) -> String -> IO ()
lastNgolem = grepNfeed 1 "https://rss.golem.de/rss.php?feed=RSS2.0" (id, id, const "")

lastNhackernews :: (String -> IO ()) -> String -> IO ()
lastNhackernews = grepNfeed 1 "https://news.ycombinator.com/rss" (id, id, const "")
