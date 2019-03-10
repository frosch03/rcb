module Algo
where

import Ascii

data Algo
    = SHA256
    deriving (Eq)

instance Ascii (Algo) where
    ascii SHA256 = "sha-256"

instance Show (Algo) where
    show SHA256 = "SHA256"

  
