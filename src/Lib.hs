module Lib
  ( build
  , test
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import qualified Dockerfile.Tester as Tester
import Prelude (FilePath, IO, (.), fmap)

build :: FilePath -> IO T.Text
build = fmap Printer.get . Parser.get

test :: FilePath -> FilePath -> IO ()
test = Tester.get
