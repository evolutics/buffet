module Main
  ( main
  ) where

import qualified Control.Monad as Monad
import qualified Data.Text.IO as T.IO
import qualified Lib
import qualified Options.Applicative as Options
import Prelude (IO, ($), (>>=), mempty, pure)

main :: IO ()
main = Monad.join $ Options.execParser (Options.info commands mempty)

commands :: Options.Parser (IO ())
commands =
  Options.hsubparser $
  Options.command "build" (Options.info (pure build) mempty)

build :: IO ()
build = Lib.build "dockerfiles" >>= T.IO.putStrLn
