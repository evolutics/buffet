module Main
  ( main
  ) where

import qualified Control.Monad as Monad
import qualified Data.Text.IO as T.IO
import qualified Lib
import qualified Options.Applicative as Options
import Prelude (FilePath, IO, ($), (<$>), mconcat, mempty)

main :: IO ()
main = Monad.join $ Options.execParser (Options.info commands mempty)

commands :: Options.Parser (IO ())
commands =
  Options.hsubparser $
  mconcat
    [ Options.command
        "build"
        (Options.info (build <$> Options.argument Options.str mempty) mempty)
    , Options.command
        "test"
        (Options.info (test <$> Options.argument Options.str mempty) mempty)
    ]

build :: FilePath -> IO ()
build = Lib.build Monad.>=> T.IO.putStrLn

test :: FilePath -> IO ()
test = Lib.test
