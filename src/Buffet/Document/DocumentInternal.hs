module Buffet.Document.DocumentInternal
  ( get
  ) where

import qualified Buffet.Document.TemplateData as TemplateData
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Paths_buffet
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

get :: Maybe FilePath -> Ir.Buffet -> IO T.Text
get customTemplate buffet = do
  template <- getTemplate customTemplate
  let (errors, result) =
        Mustache.checkedSubstitute template $ TemplateData.get buffet
  if null errors
    then pure result
    else error . unlines $ fmap show errors

getTemplate :: Maybe FilePath -> IO Mustache.Template
getTemplate customTemplate = do
  templatePath <- getTemplatePath customTemplate
  let searchSpace = [".", FilePath.takeDirectory templatePath]
  result <- Mustache.automaticCompile searchSpace templatePath
  pure $ either (error . show) id result

getTemplatePath :: Maybe FilePath -> IO FilePath
getTemplatePath = maybe getDefaultTemplatePath pure

getDefaultTemplatePath :: IO FilePath
getDefaultTemplatePath =
  Paths_buffet.getDataFileName "data/document/default_template.md.mustache"
