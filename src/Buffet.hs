module Buffet
  ( main
  ) where

import qualified Buffet.Facade as Facade
import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Options.Applicative as Options
import qualified Options.Applicative.Help.Pretty as Pretty
import Prelude
  ( FilePath
  , IO
  , Maybe(Just)
  , String
  , ($)
  , (.)
  , (<$>)
  , (<*>)
  , (>>=)
  , concatMap
  , fmap
  , mconcat
  , words
  )

main :: IO ()
main = Options.execParser root >>= Facade.get

root :: Options.ParserInfo Facade.Command
root =
  Options.info parser $ Options.progDesc "Assembles many Dockerfiles in one"
  where
    parser = versionOption <*> Options.helper <*> raw
    raw =
      Options.hsubparser $
      mconcat
        [ Options.command "assemble" assemble
        , Options.command "document" document
        , Options.command "parse" parse
        , Options.command "test" test
        ]

versionOption :: Options.Parser (a -> a)
versionOption =
  Options.infoOption "Buffet 1.0.1" $
  mconcat
    [Options.long "version", Options.helpDoc $ Just versionHelp, Options.hidden]
  where
    versionHelp =
      paragraph
        [ "Prints the program name with version on stdout"
        , "and exits successfully"
        , "according to the GNU coding standards."
        ]

assemble :: Options.ParserInfo Facade.Command
assemble =
  Options.info parser $
  Options.progDesc "Assembles a Dockerfile from a list of Dockerfiles."
  where
    parser = fmap Facade.Assemble $ Facade.AssembleArguments <$> menuOperand

menuOperand :: Options.Parser FilePath
menuOperand =
  Options.argument Options.str $
  mconcat [Options.metavar "menu_path", Options.helpDoc $ Just menuHelp]

menuHelp :: Pretty.Doc
menuHelp =
  paragraphs
    [ paragraph
        [ "File or folder that"
        , "lists the input Dockerfiles and"
        , "configures the output Dockerfile."
        ]
    , paragraph
        [ "If the path is a file,"
        , "then it is a JSON or YAML file"
        , "with the following entries:"
        ]
    , list
        [ paragraphs
            [ paragraph ["`copy_dummy_source_path`:"]
            , paragraph
                [ "File or folder"
                , "that is added as a source path"
                , "to `COPY` instructions"
                , "because at least one source must exist."
                , "Default: \"/var/empty\""
                ]
            ]
        , paragraphs
            [ paragraph ["`option_to_dish`:"]
            , paragraph
                [ "Map from `ARG` variable name to Dockerfile path."
                , "These Dockerfiles are assembled,"
                , "with their instructions conditioned"
                , "on the respective variable being nonempty."
                , "Default: {}"
                ]
            ]
        ]
    , paragraph
        [ "If the path is a folder,"
        , "then the behavior is as if a file is given"
        , "with above defaults except for the following."
        , "`option_to_dish` is a map based on the path subfolders:"
        , "for each subfolder `x`, the map has an entry"
        , "with key `x` and value `x/Dockerfile`."
        ]
    ]

paragraphs :: [Pretty.Doc] -> Pretty.Doc
paragraphs = Pretty.vsep

paragraph :: [String] -> Pretty.Doc
paragraph =
  Foldable.foldr (Pretty.</>) Pretty.line . fmap Pretty.text . concatMap words

list :: [Pretty.Doc] -> Pretty.Doc
list =
  paragraphs .
  fmap (\element -> Pretty.char '-' Pretty.<+> Pretty.align element)

document :: Options.ParserInfo Facade.Command
document = Options.info parser $ Options.progDesc "Generates documentation."
  where
    parser =
      fmap Facade.Document $
      Facade.DocumentArguments <$> templateOption <*> menuOperand
    templateOption :: Options.Parser (Maybe FilePath)
    templateOption =
      Applicative.optional
        (Options.strOption $
         mconcat
           [ Options.long "template"
           , Options.metavar "mustache_file"
           , Options.helpDoc $ Just templateHelp
           ])
    templateHelp =
      paragraph
        [ "Mustache template to render (see https://mustache.github.io)."
        , "To print the template context instead, omit this option."
        ]

parse :: Options.ParserInfo Facade.Command
parse =
  Options.info parser $
  Options.progDesc "Prints an intermediate representation for API usage."
  where
    parser = fmap Facade.Parse $ Facade.ParseArguments <$> menuOperand

test :: Options.ParserInfo Facade.Command
test = Options.info parser $ Options.progDesc "Performs health checks."
  where
    parser =
      fmap Facade.Test $
      Facade.TestArguments <$> argumentsOption <*> menuOperand
    argumentsOption :: Options.Parser (Maybe FilePath)
    argumentsOption =
      Applicative.optional
        (Options.strOption $
         mconcat
           [ Options.long "arguments"
           , Options.metavar "yaml_file"
           , Options.helpDoc $ Just argumentsHelp
           ])
    argumentsHelp =
      paragraph
        [ "JSON or YAML file with a map that"
        , "sets the Docker build arguments."
        , "Health checks are only done for dishes in this map."
        ]
