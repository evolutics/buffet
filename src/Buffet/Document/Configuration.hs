module Buffet.Document.Configuration
  ( Configuration(..)
  ) where

import Prelude (Eq, FilePath, Maybe, Ord, Show)

newtype Configuration =
  Configuration
    { template :: Maybe FilePath
    }
  deriving (Eq, Ord, Show)
