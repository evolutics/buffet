module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Data.Yaml as Yaml
import Prelude (Bool, FilePath, IO, Show, ($), mconcat, show)

data Exception =
  Exception FilePath Yaml.ParseException

instance Show Exception where
  show (Exception path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

get :: FilePath -> FilePath -> IO Bool
get buffetSource argumentsFile = do
  buffet <- ParseInternal.get buffetSource
  arguments <-
    ExceptionTools.eitherThrow (Exception argumentsFile) $
    Yaml.decodeFileEither argumentsFile
  TestInternal.get buffet arguments
