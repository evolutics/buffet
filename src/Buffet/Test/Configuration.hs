module Buffet.Test.Configuration
  ( Configuration(..)
  ) where

import qualified Data.Text as T
import Prelude (Eq, Ord, Show)

newtype Configuration =
  Configuration
    { imageId :: T.Text
    }
  deriving (Eq, Ord, Show)
