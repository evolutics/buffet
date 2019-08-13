module Dockerfile.Intermediate
  ( Box(..)
  , Utility(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Eq, Ord, Show)

newtype Box =
  Box
    { optionToUtility :: Map.Map T.Text Utility
    }
  deriving (Eq, Ord, Show)

data Utility =
  Utility
    { runs :: [Docker.Instruction T.Text]
    , extraOptionsWithDefaults :: Map.Map T.Text T.Text
    }
  deriving (Eq, Ord, Show)
