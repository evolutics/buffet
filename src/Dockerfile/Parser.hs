module Dockerfile.Parser
  ( get
  ) where

import qualified Dockerfile.Intermediate as Intermediate
import Prelude (fmap)
import qualified Utilities

get :: Utilities.Box -> Intermediate.Box
get box =
  Intermediate.Box
    {Intermediate.optionToUtility = fmap parseUtility optionToUtility}
  where
    optionToUtility = Utilities.optionToUtility box

parseUtility :: Utilities.Utility -> Intermediate.Utility
parseUtility utility =
  Intermediate.Utility
    { Intermediate.dockerfile = dockerfile
    , Intermediate.extraOptionsWithDefaults = extraOptionsWithDefaults
    }
  where
    dockerfile = Utilities.dockerfile utility
    extraOptionsWithDefaults = Utilities.extraOptionsWithDefaults utility
