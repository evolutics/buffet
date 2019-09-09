module Buffet.Toolbox.DockerTools
  ( isFrom
  ) where

import qualified Language.Docker as Docker
import Prelude (Bool(False, True))

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False
