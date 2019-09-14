module Buffet.Document.Document
  ( get
  ) where

import qualified Buffet.Document.DocumentInternal as DocumentInternal
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Data.Text as T
import Prelude (FilePath, IO, (.), fmap)

get :: FilePath -> IO T.Text
get = fmap DocumentInternal.get . ParseInternal.get
