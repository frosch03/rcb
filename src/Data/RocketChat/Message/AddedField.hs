-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.AddedField
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.AddedField
where

import Data.RocketChat.AuxiliaryParsers

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm

--       Email, Valid
type Emails
    = [(String, Bool)]

-- Datatype
data AddedField
    = AF Emails String
    deriving (Eq, Show)

-- Instances
-- Parser
pAddedField :: GenParser Char st AddedField
pAddedField = do
  string "\"fields\":{"
  (es, u) <- permute (    (\a _ b -> (a,b))
                    <$$> (try $ pEmails)
                    <||> (char ',')
                    <||> (try $ pValOfKey "username")
                    )
  char '}'
  return $ AF es u

pEmail :: GenParser Char st (String, Bool)
pEmail = do
  char '{'
  result <- permute (    (\a _ b -> (a, b))
                   <$$> (try $ pValOfKey "address")
                   <||> (char ',')
                   <||> (try $ pBoolOfKey "verified")
                   )
  char '}'
  return result

pEmails :: GenParser Char st [(String, Bool)]
pEmails = do
  string "\"emails\":["
  emails <- sepBy pEmail (char ',')
  char ']'
  return emails

-- Writer
