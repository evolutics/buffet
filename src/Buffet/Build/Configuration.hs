module Buffet.Build.Configuration
  ( Configuration(..)
  ) where

import qualified Data.Text as T
import Prelude (Eq, FilePath, Ord, Show)

data Configuration =
  Configuration
    { baseImageName :: T.Text
    , baseImageTagOption :: T.Text
    , baseImageTagValue :: T.Text
    , workdir :: FilePath
    }
  deriving (Eq, Ord, Show)
