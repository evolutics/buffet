module Buffet
  ( build
  , test
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Parse as Parse
import qualified Dockerfile.Print as Print
import qualified Dockerfile.Test as Test
import Prelude (FilePath, IO, (.), fmap)

build :: FilePath -> IO T.Text
build = fmap Print.get . Parse.get

test :: FilePath -> FilePath -> IO ()
test = Test.get
