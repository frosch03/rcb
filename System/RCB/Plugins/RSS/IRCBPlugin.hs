-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.IRCBPlugin
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.IRCBPlugin
where

-- Interface class definition
class IRCBPlugins a where
    reactiveInterface
        :: a -> ( (MVar RssConfig -> Connection -> IO Message)
              , (MVar RssConfig -> Connection -> Bool -> IO (DTime, Maybe Message))
              , (MVar RssConfig -> Connection -> Bool -> Maybe (Method.RoomId, String) -> IO Bool)
              )
    
instance IRCBPlugins ()
