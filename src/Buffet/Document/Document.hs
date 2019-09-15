module Buffet.Document.Document
  ( get
  ) where

import qualified Buffet.Document.DocumentInternal as DocumentInternal
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Control.Monad as Monad
import qualified Data.Text as T
import Prelude (FilePath, IO)

get :: FilePath -> IO T.Text
get = ParseInternal.get Monad.>=> DocumentInternal.get
