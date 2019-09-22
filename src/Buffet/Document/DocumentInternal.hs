module Buffet.Document.DocumentInternal
  ( get
  ) where

import qualified Buffet.Document.TemplateContext as TemplateContext
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Prelude
  ( FilePath
  , IO
  , Maybe
  , ($)
  , (.)
  , either
  , error
  , fmap
  , id
  , maybe
  , null
  , pure
  , show
  , unlines
  )
import qualified System.FilePath as FilePath
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Types

get :: Maybe FilePath -> Ir.Buffet -> IO T.Text
get customTemplate =
  maybe (pure . printTemplateContext) renderTemplate customTemplate .
  TemplateContext.get

printTemplateContext :: Aeson.Value -> T.Text
printTemplateContext = TextTools.decodeUtf8 . Aeson.encode

renderTemplate :: FilePath -> Aeson.Value -> IO T.Text
renderTemplate templatePath templateContext = do
  template <- getTemplate templatePath
  let (errors, result) =
        Mustache.checkedSubstitute template $ Types.mFromJSON templateContext
  if null errors
    then pure result
    else error . unlines $ fmap show errors

getTemplate :: FilePath -> IO Mustache.Template
getTemplate templatePath = do
  result <- Mustache.automaticCompile searchSpace templatePath
  pure $ either (error . show) id result
  where
    searchSpace = [".", FilePath.takeDirectory templatePath]
