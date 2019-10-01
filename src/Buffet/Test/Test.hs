module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.Configuration as Configuration
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Control.Monad as Monad
import qualified Data.Text as T
import Prelude (Bool, FilePath, IO)

get :: Configuration.Configuration -> FilePath -> IO (Bool, T.Text)
get configuration = ParseInternal.get Monad.>=> TestInternal.get configuration
