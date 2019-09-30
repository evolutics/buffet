module Buffet
  ( main
  ) where

import qualified Buffet.Facade as Facade
import qualified Control.Applicative as Applicative
import qualified Options.Applicative as Options
import Prelude (IO, ($), (<$>), (<*>), (<>), (>>=), fmap, mconcat, mempty)

main :: IO ()
main = Options.execParser (Options.info parser mempty) >>= Facade.get

parser :: Options.Parser Facade.Command
parser =
  Options.hsubparser $
  mconcat
    [ Options.command "build" (Options.info buildParser mempty)
    , Options.command "document" (Options.info documentParser mempty)
    , Options.command "parse" (Options.info parseParser mempty)
    , Options.command "test" (Options.info testParser mempty)
    ]

buildParser :: Options.Parser Facade.Command
buildParser =
  fmap Facade.Build $
  Facade.BuildArguments <$>
  Options.argument Options.str (Options.metavar "SOURCE")

documentParser :: Options.Parser Facade.Command
documentParser =
  fmap Facade.Document $
  Facade.DocumentArguments <$>
  Applicative.optional
    (Options.strOption (Options.long "template" <> Options.metavar "FILE")) <*>
  Options.argument Options.str (Options.metavar "SOURCE")

parseParser :: Options.Parser Facade.Command
parseParser =
  fmap Facade.Parse $
  Facade.ParseArguments <$>
  Options.argument Options.str (Options.metavar "SOURCE")

testParser :: Options.Parser Facade.Command
testParser =
  fmap Facade.Test $
  Facade.TestArguments <$>
  Applicative.optional
    (Options.strOption (Options.long "arguments" <> Options.metavar "ARGUMENTS")) <*>
  Options.argument Options.str (Options.metavar "SOURCE")
