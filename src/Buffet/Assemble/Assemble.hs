module Buffet.Assemble.Assemble
  ( get
  ) where

import qualified Buffet.Assemble.AssembleInternal as AssembleInternal
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Data.Text as T
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap AssembleInternal.get . ParseInternal.get
