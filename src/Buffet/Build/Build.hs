module Buffet.Build.Build
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Parse.Parse as Parse
import qualified Data.Text as T
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap BuildInternal.get . Parse.get
