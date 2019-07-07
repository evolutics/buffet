module Lib
  ( dockerfile
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dockerfile
import Prelude (($), fmap)
import qualified Utilities
import qualified UtilityEntries

dockerfile :: T.Text
dockerfile = Dockerfile.get box
  where
    box =
      Utilities.Box
        { Utilities.optionToUtility =
            Map.fromList $ fmap entryToPair UtilityEntries.get
        }
    entryToPair entry = (Utilities.option entry, Utilities.utility entry)
