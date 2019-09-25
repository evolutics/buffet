module Buffet.Document.DocumentInternal
  ( get
  ) where

import qualified Buffet.Document.TemplateContext as TemplateContext
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import Prelude
  ( Bool(True)
  , FilePath
  , IO
  , Maybe
  , Show
  , ($)
  , (.)
  , (<>)
  , fmap
  , maybe
  , pure
  , show
  , unlines
  )
import qualified System.FilePath as FilePath
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Render as Render
import qualified Text.Mustache.Types as Types
import qualified Text.Parsec as Parsec

data Exception
  = CompileException Parsec.ParseError
  | SubstituteException FilePath (NonEmpty.NonEmpty Render.SubstitutionError)

instance Show Exception where
  show (CompileException error) = show error
  show (SubstituteException path errors) =
    unlines . NonEmpty.toList . NonEmpty.cons (path <> ":") $ fmap show' errors
    where
      show' (Render.VariableNotFound name) =
        "Variable not found: " <> showName name
      show' (Render.InvalidImplicitSectionContextType valueType) =
        "Invalid implicit section context type: " <> valueType
      show' Render.InvertedImplicitSection = "Inverted implicit section"
      show' (Render.SectionTargetNotFound name) =
        "Section target not found: " <> showName name
      show' (Render.PartialNotFound path') = "Partial not found: " <> path'
      show' (Render.DirectlyRenderedValue value) =
        "Directly rendered value: " <> show value
      showName = T.unpack . T.intercalate (T.pack ".")

instance Exception.Exception Exception

get :: Maybe FilePath -> Ir.Buffet -> IO T.Text
get customTemplate =
  maybe (pure . printTemplateContext) renderTemplate customTemplate .
  TemplateContext.get

printTemplateContext :: Aeson.Value -> T.Text
printTemplateContext = TextTools.decodeUtf8 . Pretty.encodePretty' configuration
  where
    configuration =
      Pretty.defConfig
        { Pretty.confIndent = Pretty.Spaces 2
        , Pretty.confCompare = TextTools.lexicographicalCompare
        , Pretty.confTrailingNewline = True
        }

renderTemplate :: FilePath -> Aeson.Value -> IO T.Text
renderTemplate templatePath templateContext = do
  template <- getTemplate templatePath
  let (errors, result) =
        Mustache.checkedSubstitute template $ Types.mFromJSON templateContext
  maybe (pure result) (Exception.throwIO . SubstituteException templatePath) $
    NonEmpty.nonEmpty errors

getTemplate :: FilePath -> IO Mustache.Template
getTemplate templatePath =
  ExceptionTools.eitherThrow CompileException $
  Mustache.automaticCompile searchSpace templatePath
  where
    searchSpace = [".", FilePath.takeDirectory templatePath]
