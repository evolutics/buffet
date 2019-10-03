module Buffet
  ( main
  ) where

import qualified Buffet.Facade as Facade
import qualified Control.Applicative as Applicative
import qualified Options.Applicative as Options
import Prelude
  ( FilePath
  , IO
  , ($)
  , (<$>)
  , (<*>)
  , (<>)
  , (>>=)
  , fmap
  , mconcat
  , mempty
  )

main :: IO ()
main = Options.execParser (Options.info parser mempty) >>= Facade.get

parser :: Options.Parser Facade.Command
parser = Options.helper <*> versionOption <*> raw
  where
    raw =
      Options.hsubparser $
      mconcat
        [ Options.command "build" (Options.info buildParser mempty)
        , Options.command "document" (Options.info documentParser mempty)
        , Options.command "parse" (Options.info parseParser mempty)
        , Options.command "test" (Options.info testParser mempty)
        ]

versionOption :: Options.Parser (a -> a)
versionOption =
  Options.infoOption "Buffet 0.1.0" $
  mconcat
    [ Options.long "version"
    , Options.help $
      mconcat
        [ "Prints the program name with version on stdout "
        , "and exits successfully "
        , "according to the GNU coding standards."
        ]
    , Options.hidden
    ]

buildParser :: Options.Parser Facade.Command
buildParser = fmap Facade.Build $ Facade.BuildArguments <$> menuOperand

menuOperand :: Options.Parser FilePath
menuOperand =
  Options.argument Options.str (Options.metavar "menu_yaml_file_or_folder")

documentParser :: Options.Parser Facade.Command
documentParser =
  fmap Facade.Document $
  Facade.DocumentArguments <$>
  Applicative.optional
    (Options.strOption
       (Options.long "template" <> Options.metavar "mustache_file")) <*>
  menuOperand

parseParser :: Options.Parser Facade.Command
parseParser = fmap Facade.Parse $ Facade.ParseArguments <$> menuOperand

testParser :: Options.Parser Facade.Command
testParser =
  fmap Facade.Test $
  Facade.TestArguments <$>
  Applicative.optional
    (Options.strOption (Options.long "arguments" <> Options.metavar "yaml_file")) <*>
  menuOperand
