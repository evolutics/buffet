module Utilities
  ( Box(..)
  , Documentation(..)
  , Entry(..)
  , Tag(..)
  , Utility(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
    , documentation :: Documentation
    }
  deriving (Eq, Ord, Show)

data Documentation =
  Documentation
    { displayName :: T.Text
    , link :: T.Text
    , tags :: Set.Set Tag
    }
  deriving (Eq, Ord, Show)

data Tag =
  Tag
    { key :: T.Text
    , value :: T.Text
    }
  deriving (Eq, Ord, Show)

data Entry =
  Entry
    { option :: T.Text
    , utility :: Utility
    }
  deriving (Eq, Ord, Show)
