module Data.RocketChat.Message.ChangedField.NotificationType
where

import Text.ParserCombinators.Parsec

-- Datatype
data NotificationType
    = Notification
    deriving (Eq, Show)

-- Instances
instance Read (NotificationType) where
    readsPrec p s = case parse pNotificationType "" s of
                      Left s  -> error $ "error while parsing NotificationType" ++ (show s)
                      Right x -> [(x, "")]

-- Parser
pNotificationType :: GenParser Char st NotificationType
pNotificationType =
        (string "notification" >> return Notification)
    <|> (string "Notification" >> return Notification)

pEventName :: GenParser Char st (String, NotificationType)
pEventName = do
  string "\"eventName\":\""
  userId <- many1 $ noneOf "/"          ; char '/'
  notifyType <- many1 $ noneOf "\""     ; string "\""
  return (userId, read notifyType)

-- Writers
