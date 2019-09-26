module Buffet.Test.TestDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.Configuration as Configuration
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Prelude (IO, Maybe(Just, Nothing), ($), (.), mconcat)
import qualified System.Process.Typed as Process

get :: Configuration.Configuration -> Ir.Option -> Ir.Dish -> IO ()
get configuration option dish =
  case Ir.healthCheck dish of
    Nothing ->
      T.IO.hPutStrLn log $
      mconcat [T.pack "No test for dish: ", Ir.option option]
    Just command ->
      Process.runProcess_ .
      Process.setStderr (Process.useHandleOpen log) .
      Process.setStdout (Process.useHandleOpen log) $
      Process.proc
        "docker"
        [ "run"
        , T.unpack $ Configuration.imageId configuration
        , "sh"
        , "-c"
        , T.unpack command
        ]
  where
    log = Configuration.log configuration
