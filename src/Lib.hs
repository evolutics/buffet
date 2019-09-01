module Lib
  ( build
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import Prelude (FilePath, IO, (.), either, error, fmap)

build :: FilePath -> IO T.Text
build = fmap (either errors Printer.get) . Parser.get
  where
    errors :: [T.Text] -> a
    errors = error . T.unpack . T.unlines
