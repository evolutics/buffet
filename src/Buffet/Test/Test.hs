module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Control.Exception as Exception
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, Show, (.), (>>=), either, pure, show)

newtype Exception =
  Exception Yaml.ParseException

instance Show Exception where
  show (Exception exception) = Yaml.prettyPrintParseException exception

instance Exception.Exception Exception

get :: FilePath -> FilePath -> IO ()
get buffetSource argumentsFile = do
  buffet <- ParseInternal.get buffetSource
  arguments <-
    Yaml.decodeFileEither argumentsFile >>=
    either (Exception.throwIO . Exception) pure
  TestInternal.get buffet arguments
