module Buffet.Document.Document
  ( get
  ) where

import qualified Buffet.Document.Configuration as Configuration
import qualified Buffet.Document.DocumentInternal as DocumentInternal
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Control.Monad as Monad
import qualified Data.Text as T
import Prelude (FilePath, IO)

get :: Configuration.Configuration -> FilePath -> IO T.Text
get configuration =
  ParseInternal.get Monad.>=> DocumentInternal.get configuration
