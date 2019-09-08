module Buffet.Build.Configuration
  ( Configuration(..)
  ) where

import qualified Data.Text as T
import Prelude (Eq, Ord, Show)

newtype Configuration =
  Configuration
    { baseImageTagOption :: T.Text
    }
  deriving (Eq, Ord, Show)
