module Buffet.Document.DocumentTools
  ( templateDataOptions
  ) where

import qualified Data.Aeson as Aeson
import Prelude ()

templateDataOptions :: Aeson.Options
templateDataOptions =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}
