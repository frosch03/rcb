module Data.RocketChat.Message.ChangedField.ChangedFieldArgs
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

import Data.RocketChat.AuxiliaryParsers

-- Datatype
data ChangedFieldArgs
    = CFA
      { cfaTitle   :: String
      , cfaText    :: String
      , cfaPayload :: (String, String, (String, String, String), Char, String)
      }
    deriving (Eq, Show)

-- Instances
-- Parser
pChangedFieldArgs :: GenParser Char st ChangedFieldArgs
pChangedFieldArgs = do
  char '{'
  (s1, s2, pl) <- permute
         (    (\a _ b _ c -> (a,b,c))
         <$$> (try $ pValOfKey "title")
         <||> (char ',')
         <||> (try $ pValOfKey "text")
         <||> (char ',')
         <||> (try $ pPayload)
         )
  char '}'
  return $ CFA s1 s2 pl

pChangedFieldArgss :: GenParser Char st [ChangedFieldArgs]
pChangedFieldArgss = do
  string "\"args\":["
  cfas <- sepBy pChangedFieldArgs (char ',')
  char ']'
  return cfas

pPayload :: GenParser Char st (String, String, (String, String, String), Char, String)
pPayload = do
  string "\"payload\":{"
  result <- permute
         (    (\a _ b _ c _ d _ e -> (a,b,c,(head d),e))
         <$$> (try $ pValOfKey "_id")
         <||> (char ',')
         <||> (try $ pValOfKey "rid")
         <||> (char ',')
         <||> (try $ pSender)
         <||> (char ',')
         <||> (try $ pValOfKey "type")
         <||> (char ',')
         <||> (try $ pInnerMessage)
         )
  char '}'
  return result

-- Writer
