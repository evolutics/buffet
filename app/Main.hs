module Main
  ( main
  ) where

import qualified Buffet
import qualified Control.Monad as Monad
import qualified Data.Text.IO as T.IO
import qualified Options.Applicative as Options
import Prelude (FilePath, IO, ($), (<$>), (<*>), mconcat, mempty)

main :: IO ()
main = Monad.join $ Options.execParser (Options.info commands mempty)

commands :: Options.Parser (IO ())
commands =
  Options.hsubparser $
  mconcat
    [ Options.command
        "build"
        (Options.info
           (build <$> Options.argument Options.str (Options.metavar "SOURCE"))
           mempty)
    , Options.command
        "parse"
        (Options.info
           (parse <$> Options.argument Options.str (Options.metavar "SOURCE"))
           mempty)
    , Options.command
        "test"
        (Options.info
           (test <$> Options.argument Options.str (Options.metavar "SOURCE") <*>
            Options.argument Options.str (Options.metavar "ARGUMENTS"))
           mempty)
    ]

build :: FilePath -> IO ()
build = Buffet.build Monad.>=> T.IO.putStrLn

parse :: FilePath -> IO ()
parse = Buffet.parse Monad.>=> T.IO.putStrLn

test :: FilePath -> FilePath -> IO ()
test = Buffet.test
