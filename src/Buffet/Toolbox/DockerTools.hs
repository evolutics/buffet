module Buffet.Toolbox.DockerTools
  ( isArg
  , isFrom
  ) where

import qualified Language.Docker as Docker
import Prelude (Bool(False, True))

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False
