module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.Configuration as Configuration
import qualified Buffet.Test.ParseArguments as ParseArguments
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Data.Text as T
import Prelude (Bool, FilePath, IO)

get :: Configuration.Configuration -> FilePath -> IO (Bool, T.Text)
get configuration buffetSource = do
  buffet <- ParseInternal.get buffetSource
  arguments <- ParseArguments.get configuration
  TestInternal.get buffet arguments
