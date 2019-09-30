module Buffet.Facade
  ( Command(..)
  , get
  ) where

import qualified Buffet.Build.Build as Build
import qualified Buffet.Document.Document as Document
import qualified Buffet.Parse.Parse as Parse
import qualified Buffet.Test.Test as Test
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Prelude (Bool, Eq, FilePath, IO, Maybe, Ord, Show, (>>=), uncurry)
import qualified System.Exit as Exit

data Command
  = Build FilePath
  | Document (Maybe FilePath) FilePath
  | Parse FilePath
  | Test (Maybe FilePath) FilePath
  deriving (Eq, Ord, Show)

get :: Command -> IO ()
get command =
  case command of
    Build buffetSource -> build buffetSource
    Document template buffetSource -> document template buffetSource
    Parse buffetSource -> parse buffetSource
    Test argumentsFile buffetSource -> test argumentsFile buffetSource

build :: FilePath -> IO ()
build buffetSource = Build.get buffetSource >>= T.IO.putStr

document :: Maybe FilePath -> FilePath -> IO ()
document template buffetSource =
  Document.get template buffetSource >>= T.IO.putStr

parse :: FilePath -> IO ()
parse buffetSource = Parse.get buffetSource >>= T.IO.putStr

test :: Maybe FilePath -> FilePath -> IO ()
test argumentsFile buffetSource =
  Test.get argumentsFile buffetSource >>= uncurry exitPrintingStdout

exitPrintingStdout :: Bool -> T.Text -> IO a
exitPrintingStdout isSuccess result = do
  T.IO.putStr result
  if isSuccess
    then Exit.exitSuccess
    else Exit.exitFailure
