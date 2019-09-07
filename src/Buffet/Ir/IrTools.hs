module Buffet.Ir.IrTools
  ( mapOrderedEntries
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude ((<$>), uncurry)

mapOrderedEntries :: (T.Text -> Ir.Dish -> a) -> Ir.Buffet -> [a]
mapOrderedEntries function buffet = uncurry function <$> Map.toAscList map
  where
    map = Ir.optionToDish buffet
