module Buffet.Document.DocumentInternal
  ( get
  ) where

import qualified Buffet.Document.TemplateContext as TemplateContext
import qualified Buffet.Ir.Ir as Ir
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
  , either
  , error
  , fmap
  , id
  , maybe
  , pure
  , show
  , unlines
  )
import qualified System.FilePath as FilePath
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Render as Render
import qualified Text.Mustache.Types as Types

newtype Exception =
  SubstituteException (NonEmpty.NonEmpty Render.SubstitutionError)

instance Show Exception where
  show (SubstituteException errors) =
    unlines . NonEmpty.toList $ fmap show' errors
    where
      show' (Render.VariableNotFound name) =
        "Variable not found: " <> showName name
      show' (Render.InvalidImplicitSectionContextType valueType) =
        "Invalid implicit section context type: " <> valueType
      show' Render.InvertedImplicitSection = "Inverted implicit section"
      show' (Render.SectionTargetNotFound name) =
        "Section target not found: " <> showName name
      show' (Render.PartialNotFound path) = "Partial not found: " <> path
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
  maybe (pure result) (Exception.throwIO . SubstituteException) $
    NonEmpty.nonEmpty errors

getTemplate :: FilePath -> IO Mustache.Template
getTemplate templatePath = do
  result <- Mustache.automaticCompile searchSpace templatePath
  pure $ either (error . show) id result
  where
    searchSpace = [".", FilePath.takeDirectory templatePath]
