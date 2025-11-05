module Types (TestOrReal(..), Variant(..)) where

import Prelude
data TestOrReal = Test | Real deriving (Read, Show)
data Variant = Star1 | Star2 deriving (Read, Show)