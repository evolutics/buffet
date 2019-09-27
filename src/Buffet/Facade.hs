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
  | Test FilePath FilePath
  deriving (Eq, Ord, Show)

get :: Command -> IO ()
get command =
  case command of
    Build buffetSource -> Build.get buffetSource >>= T.IO.putStr
    Document template buffetSource ->
      Document.get template buffetSource >>= T.IO.putStr
    Parse buffetSource -> Parse.get buffetSource >>= T.IO.putStr
    Test buffetSource argumentsFile ->
      Test.get buffetSource argumentsFile >>= uncurry exitPrintingStdout

exitPrintingStdout :: Bool -> T.Text -> IO a
exitPrintingStdout isSuccess result = do
  T.IO.putStr result
  if isSuccess
    then Exit.exitSuccess
    else Exit.exitFailure
