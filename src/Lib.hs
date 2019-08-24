module Lib
  ( dockerfile
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import Prelude (Either, FilePath, IO, ($), (.), either, error, fmap)

dockerfile :: FilePath -> IO T.Text
dockerfile = generate . Parser.get

generate :: IO (Either [T.Text] Intermediate.Box) -> IO T.Text
generate = fmap $ either errors Printer.get
  where
    errors :: [T.Text] -> a
    errors = error . T.unpack . T.unlines
