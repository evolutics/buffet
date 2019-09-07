module Dockerfile.Validate
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Ir as Ir
import qualified Dockerfile.Validations.EachStageHasOptionArg as EachStageHasOptionArg
import Prelude ()

get :: Ir.Box -> [T.Text]
get = EachStageHasOptionArg.get
