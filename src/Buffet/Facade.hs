module Buffet.Facade
  ( build
  , parse
  , test
  ) where

import qualified Buffet.Build.Build as Build
import qualified Buffet.Parse.Parse as Parse
import qualified Buffet.Test.Test as Test
import qualified Data.Text as T
import Prelude (FilePath, IO)

build :: FilePath -> IO T.Text
build = Build.get

parse :: FilePath -> IO T.Text
parse = Parse.get

test :: FilePath -> FilePath -> IO ()
test = Test.get
