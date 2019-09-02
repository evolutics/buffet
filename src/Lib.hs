module Lib
  ( build
  , test
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import Prelude (FilePath, IO, (.), fmap, undefined)

build :: FilePath -> IO T.Text
build = fmap Printer.get . Parser.get

test :: FilePath -> FilePath -> IO ()
test = undefined
