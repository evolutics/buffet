module Buffet.Ir.IrTools
  ( mapOrderedEntries
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude ((<$>), uncurry)

mapOrderedEntries :: (T.Text -> Ir.Utility -> a) -> Ir.Box -> [a]
mapOrderedEntries function box = uncurry function <$> Map.toAscList map
  where
    map = Ir.optionToUtility box
