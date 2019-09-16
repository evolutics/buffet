module Buffet
  ( main
  ) where

import qualified Buffet.Facade as Facade
import qualified Options.Applicative as Options
import Prelude (IO, ($), (<$>), (<*>), (>>=), mconcat, mempty)

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
  Facade.Build <$> Options.argument Options.str (Options.metavar "SOURCE")

documentParser :: Options.Parser Facade.Command
documentParser =
  Facade.Document <$> Options.argument Options.str (Options.metavar "SOURCE")

parseParser :: Options.Parser Facade.Command
parseParser =
  Facade.Parse <$> Options.argument Options.str (Options.metavar "SOURCE")

testParser :: Options.Parser Facade.Command
testParser =
  Facade.Test <$> Options.argument Options.str (Options.metavar "SOURCE") <*>
  Options.argument Options.str (Options.metavar "ARGUMENTS")
