module Buffet.Document.Document
  ( get
  ) where

import qualified Buffet.Document.DocumentInternal as DocumentInternal
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Control.Monad as Monad
import qualified Data.Text as T
import Prelude (FilePath, IO, Maybe)

get :: Maybe FilePath -> FilePath -> IO T.Text
get template = ParseInternal.get Monad.>=> DocumentInternal.get template
