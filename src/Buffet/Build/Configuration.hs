module Buffet.Build.Configuration
  ( Configuration(..)
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (Bool, Eq, FilePath, Ord, Show)

data Configuration =
  Configuration
    { baseImageName :: T.Text
    , baseImageTagOption :: Ir.Option
    , baseImageTagValue :: T.Text
    , workdir :: FilePath
    , optimize :: Bool
    }
  deriving (Eq, Ord, Show)
