module Buffet.Build.HasArgInstructionWithName
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Language.Docker as Docker
import Prelude (Bool(False), (==), any)

get :: Ir.Option -> Ir.DockerfilePart -> Bool
get option = any isArgWithName
  where
    isArgWithName (Docker.Arg name _) = name == optionName
    isArgWithName _ = False
    optionName = Ir.option option
