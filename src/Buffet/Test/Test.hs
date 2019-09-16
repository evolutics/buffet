module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO)

get :: FilePath -> FilePath -> IO ()
get buffetSource argumentsFile = do
  buffet <- ParseInternal.get buffetSource
  arguments <- Yaml.decodeFileThrow argumentsFile
  TestInternal.get buffet arguments
