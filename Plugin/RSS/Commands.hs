module Plugin.RSS.Commands where

import Plugin.RSS.Auxiliary


-- type RssConfig = [FeedDescriptor]

data RssConfig
    = RssConfig
      { feeds :: [(String, FeedDescriptor)]
      , pushs :: PushDescriptors
      }

data FeedDescriptor
    = Feed
      { feedUrl         :: String
      , feedTransformer :: (String -> String, String -> String, String -> String)
      }

data PushDescriptors
    = Push
      { pushFeedIntoRoomss :: [(FeedDescriptor, [String])]
      , pushInterval       :: Int
      }

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
            [ (Feed "https://news.ycombinator.com/rss" (id, id, const ""), ["5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR"])
            , (Feed "https://blog.fefe.de/rss.xml?html" (filter (not . flip elem ['`', '"']), id, const ""), ["5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR"])
            ]
            (6 * 60)
    }
