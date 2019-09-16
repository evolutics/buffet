module Buffet.Facade
  ( build
  , document
  , parse
  , test
  ) where

import qualified Buffet.Build.Build as Build
import qualified Buffet.Document.Document as Document
import qualified Buffet.Parse.Parse as Parse
import qualified Buffet.Test.Test as Test
import qualified Control.Monad as Monad
import qualified Data.Text.IO as T.IO
import Prelude (FilePath, IO)

build :: FilePath -> IO ()
build = Build.get Monad.>=> T.IO.putStr

document :: FilePath -> IO ()
document = Document.get Monad.>=> T.IO.putStr

parse :: FilePath -> IO ()
parse = Parse.get Monad.>=> T.IO.putStr

test :: FilePath -> FilePath -> IO ()
test = Test.get
