module Buffet.Build.ArgInstruction
  ( ArgInstruction(..)
  ) where

import qualified Data.Text as T
import Prelude (Eq, Maybe, Ord, Show)

data ArgInstruction =
  ArgInstruction
    { name :: T.Text
    , defaultValue :: Maybe T.Text
    }
  deriving (Eq, Ord, Show)
