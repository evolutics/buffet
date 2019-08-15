module Dockerfile
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import Prelude ((.), either, error)
import qualified Utilities

get :: Utilities.Box -> T.Text
get = either errors Printer.get . Parser.get
  where
    errors :: [T.Text] -> a
    errors = error . T.unpack . T.unlines
