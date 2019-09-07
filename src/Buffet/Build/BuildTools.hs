module Buffet.Build.BuildTools
  ( isLabel
  ) where

import qualified Language.Docker as Docker
import Prelude (Bool(False, True))

isLabel :: Docker.Instruction a -> Bool
isLabel (Docker.Label _) = True
isLabel _ = False
