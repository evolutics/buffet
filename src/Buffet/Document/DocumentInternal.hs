module Buffet.Document.DocumentInternal
  ( get
  ) where

import qualified Buffet.Document.DefaultTemplate as DefaultTemplate
import qualified Buffet.Document.TemplateData as TemplateData
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (($), (.), error, fmap, null, show, unlines)
import qualified Text.Mustache as Mustache

get :: Ir.Buffet -> T.Text
get buffet =
  if null errors
    then result
    else error . unlines $ fmap show errors
  where
    (errors, result) =
      Mustache.checkedSubstitute DefaultTemplate.get $ TemplateData.get buffet
