module Utilities
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

newtype Utility =
  Utility
    { dockerfile :: T.Text
    }
  deriving (Eq, Ord, Show)
