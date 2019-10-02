module Buffet.Parse.Menu
  ( Menu(..)
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (Eq, FilePath, Ord, Show)

newtype Menu =
  Menu
    { optionToDish :: Map.Map Ir.Option FilePath
    }
  deriving (Eq, Ord, Show)
