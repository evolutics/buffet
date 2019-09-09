module Buffet.Parse.ParseMetadata
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Language.Docker as Docker
import Prelude ()

get :: Docker.Dockerfile -> Ir.Metadata
get _ = Ir.Metadata {}
