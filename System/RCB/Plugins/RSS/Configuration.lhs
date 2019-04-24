> module System.RCB.Plugins.RSS.Configuration where
>
> import System.RCB.Plugins.RSS.RssConfig.Datatype
> import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
> import System.RCB.Plugins.RSS.RssConfig.PushDescriptors
> import Data.RocketChat.Message.Datatype

> import System.RCB.Plugins.RSS.Auxiliary

This is the configuration file of the RSS plugin.

Here the initial definition of the feeds and feed commands:

> rssConfig :: RssConfig
> rssConfig =
>     RssConfig
>     { feeds =
>           [ ("xkcd", Feed "https://www.xkcd.com/rss.xml" (id, id, grepImgUrl))
>           , ("fefe", Feed "https://blog.fefe.de/rss.xml?html" (filter (not . flip elem ['`', '"']), id, const ""))
>           , ("heise", Feed "http://www.heise.de/newsticker/heise.rdf" (id, id, const ""))
>           , ("golem", Feed "https://rss.golem.de/rss.php?feed=RSS2.0" (id, id, const ""))
>           , ("hackernews", Feed "https://news.ycombinator.com/rss" (id, id, const ""))
>           ]
> 
>     , pushs =
>           Push
>             [ (Feed "https://news.ycombinator.com/rss" (id, id, const ""), [Room "frosch03" "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" Direct])
>             , (Feed "https://www.xkcd.com/rss.xml" (id, id, grepImgUrl), [Room "frosch03" "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" Direct])
>             , (Feed "https://blog.fefe.de/rss.xml?html" (filter (not . flip elem ['`', '"']), id, const ""), [Room "frosch03" "5gBGjzg9oHMZb9DpRdNfBQiWGorDmHwWXR" Direct])
>             ]
>             (6 * 60)
>     }
