module Buffet.Parse.Parse
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Parse.Print as Print
import qualified Data.Text as T
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap Print.get . ParseInternal.get
