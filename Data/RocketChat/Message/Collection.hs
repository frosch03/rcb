module Data.RocketChat.Message.Collection
where

import Text.ParserCombinators.Parsec

-- Datatype
data Collection
    = StreamNotifyUser
    deriving (Eq, Show)

-- Instances
instance Read (Collection) where
    readsPrec p s = case parse pCollection "" s of
                      Left s  -> error $ "error while parsing Collection" ++ (show s)
                      Right x -> [(x, "")]

-- Parser
pCollection :: GenParser Char st Collection
pCollection
    =     (string "stream-notify-user" >> return StreamNotifyUser)
      <|> (string "Stream-Notify-User" >> return StreamNotifyUser)

-- Writer
