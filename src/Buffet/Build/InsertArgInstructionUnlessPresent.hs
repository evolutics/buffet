module Buffet.Build.InsertArgInstructionUnlessPresent
  ( get
  ) where

import qualified Buffet.Build.ArgInstruction as ArgInstruction
import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), ($), (<>), (==), any, span)

get :: ArgInstruction.ArgInstruction -> Ir.DockerfilePart -> Ir.DockerfilePart
get argInstruction stage = firstFroms <> preparedAfterFirstFroms
  where
    (firstFroms, afterFirstFroms) = span isFrom stage
    preparedAfterFirstFroms =
      if hasArgWithName name afterFirstFroms
        then afterFirstFroms
        else List.insert arg firstArgs <> afterFirstArgs
    name = ArgInstruction.name argInstruction
    arg = Docker.Arg name $ ArgInstruction.defaultValue argInstruction
    (firstArgs, afterFirstArgs) = span isArg afterFirstFroms

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

hasArgWithName :: T.Text -> Ir.DockerfilePart -> Bool
hasArgWithName name = any isArgWithName
  where
    isArgWithName (Docker.Arg name' _) = name' == name
    isArgWithName _ = False

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False
