module Data.RocketChat.Message.ResultField.AuthType
where

import Text.ParserCombinators.Parsec

-- Datatype
data AuthType
    = Password
    deriving (Eq, Show)


-- Instances
instance Read (AuthType) where
    readsPrec p s = case parse pAuthType "" s of
                      Left s  -> error $ "error while parsing AuthType" ++ (show s)
                      Right x -> [(x, "")]

-- Parser
pAuthType :: GenParser Char st AuthType
pAuthType
    =     (string "password" >> return Password)
      <|> (string "Password" >> return Password)


--- Write



  
