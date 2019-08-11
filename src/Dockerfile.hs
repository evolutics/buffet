module Dockerfile
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import Prelude ((.))
import qualified Utilities

get :: Utilities.Box -> T.Text
get = Printer.get . Parser.get
