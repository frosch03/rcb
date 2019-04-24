module Data.RocketChat.Message.ChangedField.Parser
where

import Data.RocketChat.Message.ChangedField.Datatype
import Data.RocketChat.Message.ChangedField.ChangedFieldArgs
import Data.RocketChat.Message.ChangedField.NotificationType

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

pChangedField :: GenParser Char st ChangedField
pChangedField = do
  string "\"fields\":{"
  (s, n) <- permute
         (    (\a _ b -> (a,b))
         <$$> (try $ pEventName)
         <||> (char ',')
         <||> (try $ pChangedFieldArgss)
         )
  char '}'
  return $ CF s n
