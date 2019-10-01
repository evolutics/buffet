module Buffet.Test.ParseArguments
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.Configuration as Configuration
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, Show, ($), (.), maybe, mconcat, pure, show)

data Exception =
  Exception FilePath Yaml.ParseException

instance Show Exception where
  show (Exception path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

get :: Configuration.Configuration -> IO (Map.Map Ir.Option T.Text)
get =
  maybe
    (pure Map.empty)
    (\file ->
       ExceptionTools.eitherThrow (Exception file) $ Yaml.decodeFileEither file) .
  Configuration.arguments
