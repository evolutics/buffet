module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, (<$>))

get :: FilePath -> FilePath -> IO ()
get buffetSource argumentsFile = do
  buffet <- ParseInternal.get buffetSource
  arguments <- Map.mapKeys Ir.Option <$> Yaml.decodeFileThrow argumentsFile
  TestInternal.get buffet arguments
