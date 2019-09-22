module Buffet.Document.TemplateData
  ( get
  ) where

import qualified Buffet.Document.TemplateBuffet as TemplateBuffet
import qualified Buffet.Ir.Ir as Ir
import Prelude ((.))
import qualified Text.Mustache.Types as Types

get :: Ir.Buffet -> Types.Value
get = Types.mFromJSON . TemplateBuffet.get
