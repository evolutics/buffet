module Buffet.Test.TestDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.Configuration as Configuration
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Prelude (IO, Maybe(Just, Nothing), ($), mconcat)
import qualified System.IO as IO
import qualified System.Process.Typed as Process

get :: Configuration.Configuration -> Ir.Option -> Ir.Dish -> IO ()
get configuration option dish =
  case Ir.testCommand dish of
    Nothing ->
      putStderrLine $ mconcat [T.pack "No test for dish: ", Ir.option option]
    Just testCommand ->
      Process.runProcess_ $
      Process.proc
        "docker"
        [ "run"
        , T.unpack $ Configuration.imageId configuration
        , "sh"
        , "-c"
        , T.unpack testCommand
        ]

putStderrLine :: T.Text -> IO ()
putStderrLine = T.IO.hPutStrLn IO.stderr
