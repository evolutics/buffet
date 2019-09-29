module Buffet.Test.TestSetup
  ( TestSetup(..)
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (Eq, Maybe, Show)
import qualified System.IO as IO

data TestSetup =
  TestSetup
    { log :: IO.Handle
    , image :: T.Text
    , option :: Ir.Option
    , optionValue :: Maybe T.Text
    , dish :: Ir.Dish
    }
  deriving (Eq, Show)
