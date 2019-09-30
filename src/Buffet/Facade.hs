module Buffet.Facade
  ( BuildArguments(..)
  , Command(..)
  , DocumentArguments(..)
  , ParseArguments(..)
  , TestArguments(..)
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
  = Build BuildArguments
  | Document DocumentArguments
  | Parse ParseArguments
  | Test TestArguments
  deriving (Eq, Ord, Show)

newtype BuildArguments =
  BuildArguments
    { buildBuffet :: FilePath
    }
  deriving (Eq, Ord, Show)

data DocumentArguments =
  DocumentArguments
    { documentTemplate :: Maybe FilePath
    , documentBuffet :: FilePath
    }
  deriving (Eq, Ord, Show)

newtype ParseArguments =
  ParseArguments
    { parseBuffet :: FilePath
    }
  deriving (Eq, Ord, Show)

data TestArguments =
  TestArguments
    { testArguments :: Maybe FilePath
    , testBuffet :: FilePath
    }
  deriving (Eq, Ord, Show)

get :: Command -> IO ()
get command =
  case command of
    Build arguments -> build arguments
    Document arguments -> document arguments
    Parse arguments -> parse arguments
    Test arguments -> test arguments

build :: BuildArguments -> IO ()
build arguments = Build.get (buildBuffet arguments) >>= T.IO.putStr

document :: DocumentArguments -> IO ()
document arguments =
  Document.get (documentTemplate arguments) (documentBuffet arguments) >>=
  T.IO.putStr

parse :: ParseArguments -> IO ()
parse arguments = Parse.get (parseBuffet arguments) >>= T.IO.putStr

test :: TestArguments -> IO ()
test arguments =
  Test.get (testArguments arguments) (testBuffet arguments) >>=
  uncurry exitPrintingStdout

exitPrintingStdout :: Bool -> T.Text -> IO a
exitPrintingStdout isSuccess result = do
  T.IO.putStr result
  if isSuccess
    then Exit.exitSuccess
    else Exit.exitFailure
