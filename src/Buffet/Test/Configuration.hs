module Buffet.Test.Configuration
  ( Configuration(..)
  ) where

import qualified Data.Text as T
import Prelude (Eq, Show)
import qualified System.IO as IO

data Configuration =
  Configuration
    { imageId :: T.Text
    , log :: IO.Handle
    }
  deriving (Eq, Show)
