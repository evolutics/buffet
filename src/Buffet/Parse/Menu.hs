module Buffet.Parse.Menu
  ( Menu(..)
  , defaultMenu
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (Bool(True), Eq, FilePath, Ord, Show)

data Menu =
  Menu
    { optimize :: Bool
    , optionToDish :: Map.Map Ir.Option FilePath
    }
  deriving (Eq, Ord, Show)

defaultMenu :: Menu
defaultMenu = Menu {optimize = True, optionToDish = Map.empty}
