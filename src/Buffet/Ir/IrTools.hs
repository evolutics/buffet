module Buffet.Ir.IrTools
  ( mapOrderedEntries
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude ((<$>), uncurry)

mapOrderedEntries :: (Ir.Option -> Ir.Dish -> a) -> Ir.Buffet -> [a]
mapOrderedEntries function buffet =
  uncurry function <$> Map.toAscList optionToDish
  where
    optionToDish = Ir.optionToDish buffet
