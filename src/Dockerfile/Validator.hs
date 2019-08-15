module Dockerfile.Validator
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Validators.EachStageHasOptionArg as EachStageHasOptionArg
import Prelude ()

get :: Intermediate.Box -> [T.Text]
get = EachStageHasOptionArg.get
