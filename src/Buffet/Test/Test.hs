module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, Show, ($), show)

newtype Exception =
  Exception Yaml.ParseException

instance Show Exception where
  show (Exception exception) = Yaml.prettyPrintParseException exception

instance Exception.Exception Exception

get :: FilePath -> FilePath -> IO ()
get buffetSource argumentsFile = do
  buffet <- ParseInternal.get buffetSource
  arguments <-
    ExceptionTools.eitherThrow Exception $ Yaml.decodeFileEither argumentsFile
  TestInternal.get buffet arguments
