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
