module Buffet.Test.TestDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Prelude (IO, Maybe(Just, Nothing), ($), mconcat)
import qualified System.IO as IO
import qualified System.Process.Typed as Process

get :: T.Text -> Ir.Option -> Ir.Dish -> IO ()
get imageId option dish =
  case Ir.testCommand dish of
    Nothing ->
      putStderrLine $ mconcat [T.pack "No test for dish: ", Ir.option option]
    Just testCommand ->
      Process.runProcess_ $
      Process.proc
        "docker"
        ["run", T.unpack imageId, "sh", "-c", T.unpack testCommand]

putStderrLine :: T.Text -> IO ()
putStderrLine = T.IO.hPutStrLn IO.stderr
