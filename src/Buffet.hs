module Buffet
  ( build
  , test
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Build as Build
import qualified Dockerfile.Test as Test
import Prelude (FilePath, IO)

build :: FilePath -> IO T.Text
build = Build.get

test :: FilePath -> FilePath -> IO ()
test = Test.get
