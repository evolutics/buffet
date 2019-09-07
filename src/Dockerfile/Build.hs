module Dockerfile.Build
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parse as Parse
import qualified Dockerfile.Print as Print
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap Print.get . Parse.get
