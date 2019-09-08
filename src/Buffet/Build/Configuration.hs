module Buffet.Build.Configuration
  ( Configuration(..)
  ) where

import qualified Data.Text as T
import Prelude (Eq, Ord, Show)

data Configuration =
  Configuration
    { baseImageTagOption :: T.Text
    , baseImageTagValue :: T.Text
    }
  deriving (Eq, Ord, Show)
