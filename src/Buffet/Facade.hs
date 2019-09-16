module Buffet.Facade
  ( Command(..)
  , get
  ) where

import qualified Buffet.Build.Build as Build
import qualified Buffet.Document.Document as Document
import qualified Buffet.Parse.Parse as Parse
import qualified Buffet.Test.Test as Test
import qualified Data.Text.IO as T.IO
import Prelude (Eq, FilePath, IO, Ord, Show, (>>=))

data Command
  = Build FilePath
  | Document FilePath
  | Parse FilePath
  | Test FilePath FilePath
  deriving (Eq, Ord, Show)

get :: Command -> IO ()
get command =
  case command of
    Build buffetSource -> Build.get buffetSource >>= T.IO.putStr
    Document buffetSource -> Document.get buffetSource >>= T.IO.putStr
    Parse buffetSource -> Parse.get buffetSource >>= T.IO.putStr
    Test buffetSource argumentsFile -> Test.get buffetSource argumentsFile
