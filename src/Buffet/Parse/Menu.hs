module Buffet.Parse.Menu
  ( Menu(..)
  , defaultMenu
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (Eq, FilePath, Ord, Show)

newtype Menu =
  Menu
    { optionToDish :: Map.Map Ir.Option FilePath
    }
  deriving (Eq, Ord, Show)

defaultMenu :: Menu
defaultMenu = Menu {optionToDish = Map.empty}
