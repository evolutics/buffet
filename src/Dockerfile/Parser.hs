module Dockerfile.Parser
  ( get
  ) where

import qualified Dockerfile.Intermediate as Intermediate
import Prelude ()
import qualified Utilities

get :: Utilities.Box -> Intermediate.Box
get box = Intermediate.Box {Intermediate.optionToUtility = optionToUtility}
  where
    optionToUtility = Utilities.optionToUtility box
