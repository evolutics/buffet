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
  , ($)
  , (.)
  , either
  , error
  , fmap
  , id
  , null
  , pure
  , show
  , unlines
  )
import qualified System.FilePath as FilePath
import qualified Text.Mustache as Mustache

get :: Ir.Buffet -> IO T.Text
get buffet = do
  template <- getTemplate
  let (errors, result) =
        Mustache.checkedSubstitute template $ TemplateData.get buffet
  if null errors
    then pure result
    else error . unlines $ fmap show errors

getTemplate :: IO Mustache.Template
getTemplate = do
  templatePath <- getTemplatePath
  let searchSpace = [FilePath.takeDirectory templatePath]
  result <- Mustache.automaticCompile searchSpace templatePath
  pure $ either (error . show) id result

getTemplatePath :: IO FilePath
getTemplatePath =
  Paths_buffet.getDataFileName "data/document/default_template.md.mustache"
