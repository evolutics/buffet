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
main = Options.execParser root >>= Facade.get

root :: Options.ParserInfo Facade.Command
root = Options.info parser mempty
  where
    parser = helpOption <*> versionOption <*> raw
    raw =
      Options.hsubparser $
      mconcat
        [ Options.command "build" build
        , Options.command "document" document
        , Options.command "parse" parse
        , Options.command "test" test
        ]

helpOption :: Options.Parser (a -> a)
helpOption =
  Options.abortOption Options.ShowHelpText $
  mconcat
    [ Options.long "help"
    , Options.short 'h'
    , Options.help "Prints this help text on stdout and exits successfully."
    , Options.hidden
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

build :: Options.ParserInfo Facade.Command
build = Options.info parser mempty
  where
    parser = fmap Facade.Build $ Facade.BuildArguments <$> menuOperand

menuOperand :: Options.Parser FilePath
menuOperand = Options.argument Options.str (Options.metavar "menu")

document :: Options.ParserInfo Facade.Command
document = Options.info parser mempty
  where
    parser =
      fmap Facade.Document $
      Facade.DocumentArguments <$>
      Applicative.optional
        (Options.strOption
           (Options.long "template" <> Options.metavar "mustache_file")) <*>
      menuOperand

parse :: Options.ParserInfo Facade.Command
parse = Options.info parser mempty
  where
    parser = fmap Facade.Parse $ Facade.ParseArguments <$> menuOperand

test :: Options.ParserInfo Facade.Command
test = Options.info parser mempty
  where
    parser =
      fmap Facade.Test $
      Facade.TestArguments <$>
      Applicative.optional
        (Options.strOption
           (Options.long "arguments" <> Options.metavar "yaml_file")) <*>
      menuOperand
