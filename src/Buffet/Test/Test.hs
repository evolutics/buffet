module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseInternal as ParseInternal
import qualified Buffet.Test.Configuration as Configuration
import qualified Buffet.Test.TestInternal as TestInternal
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Prelude
  ( Bool
  , FilePath
  , IO
  , Maybe
  , Show
  , ($)
  , maybe
  , mconcat
  , pure
  , show
  )

data Exception =
  Exception FilePath Yaml.ParseException

instance Show Exception where
  show (Exception path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

get :: Configuration.Configuration -> FilePath -> IO (Bool, T.Text)
get configuration buffetSource = do
  buffet <- ParseInternal.get buffetSource
  arguments <- getArguments $ Configuration.arguments configuration
  TestInternal.get buffet arguments

getArguments :: Maybe FilePath -> IO (Map.Map Ir.Option T.Text)
getArguments =
  maybe
    (pure Map.empty)
    (\file ->
       ExceptionTools.eitherThrow (Exception file) $ Yaml.decodeFileEither file)
