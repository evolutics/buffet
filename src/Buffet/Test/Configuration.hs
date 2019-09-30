module Buffet.Test.Configuration
  ( Configuration(..)
  ) where

import Prelude (Eq, FilePath, Maybe, Ord, Show)

newtype Configuration =
  Configuration
    { arguments :: Maybe FilePath
    }
  deriving (Eq, Ord, Show)
