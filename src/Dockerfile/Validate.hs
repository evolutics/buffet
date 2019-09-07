module Dockerfile.Validate
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Validations.EachStageHasOptionArg as EachStageHasOptionArg
import Prelude ()

get :: Intermediate.Box -> [T.Text]
get = EachStageHasOptionArg.get
