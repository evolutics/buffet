module Dockerfile.Intermediate
  ( Box(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Eq, Ord, Show)
import qualified Utilities

newtype Box =
  Box
    { optionToUtility :: Map.Map T.Text Utilities.Utility
    }
  deriving (Eq, Ord, Show)
