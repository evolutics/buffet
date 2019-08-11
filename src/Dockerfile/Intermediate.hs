module Dockerfile.Intermediate
  ( Box(..)
  , Utility(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Eq, Ord, Show)

newtype Box =
  Box
    { optionToUtility :: Map.Map T.Text Utility
    }
  deriving (Eq, Ord, Show)

data Utility =
  Utility
    { dockerfile :: T.Text
    , extraOptionsWithDefaults :: Map.Map T.Text T.Text
    }
  deriving (Eq, Ord, Show)
