module Dockerfile.Build
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.BuildInternal as BuildInternal
import qualified Dockerfile.Parse as Parse
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap BuildInternal.get . Parse.get
