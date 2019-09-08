module Buffet.Build.Build
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Data.Text as T
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap BuildInternal.get . ParseInternal.get
