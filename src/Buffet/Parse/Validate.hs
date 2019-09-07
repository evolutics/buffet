module Buffet.Parse.Validate
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Validations.EachStageHasOptionArg as EachStageHasOptionArg
import qualified Data.Text as T
import Prelude ()

get :: Ir.Buffet -> [T.Text]
get = EachStageHasOptionArg.get
