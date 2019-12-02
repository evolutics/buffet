module Buffet.Test.TestSetup
  ( TestSetup(..)
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (Eq, Show)
import qualified System.IO as IO

data TestSetup =
  TestSetup
    { log :: IO.Handle
    , image :: T.Text
    , option :: Ir.Option
    , optionValue :: T.Text
    , dish :: Ir.Dish
    }
  deriving (Eq, Show)
