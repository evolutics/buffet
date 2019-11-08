module Buffet.Parse.ParseInternal
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Menu as Menu
import qualified Buffet.Parse.ParseDish as ParseDish
import qualified Buffet.Parse.ParseMenu as ParseMenu
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Language.Docker as Docker
import qualified Language.Docker.Parser as Parser
import Prelude (FilePath, IO, Show, ($), (.), fmap, pure, show)

newtype Exception =
  Exception Parser.Error

instance Show Exception where
  show (Exception error) = Docker.errorBundlePretty error

instance Exception.Exception Exception

get :: FilePath -> IO Ir.Buffet
get = ParseMenu.get Monad.>=> parseBuffet

parseBuffet :: Menu.Menu -> IO Ir.Buffet
parseBuffet menu = do
  optionToDish <-
    ExceptionTools.sequenceAccumulatingExceptions . fmap ParseDish.get $
    Menu.optionToDish menu
  pure Ir.Buffet {Ir.optionToDish = optionToDish}
