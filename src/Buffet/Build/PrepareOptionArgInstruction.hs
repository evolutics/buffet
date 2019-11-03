module Buffet.Build.PrepareOptionArgInstruction
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), Maybe(Nothing), (<>), elem, span)

get :: Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
get option stage = firstFroms <> preparedAfterFirstFroms
  where
    (firstFroms, afterFirstFroms) = span isFrom stage
    preparedAfterFirstFroms =
      if optionArg `elem` afterFirstFroms
        then afterFirstFroms
        else List.insert optionArg firstArgs <> afterFirstArgs
    optionArg = Docker.Arg (Ir.option option) Nothing
    (firstArgs, afterFirstArgs) = span isArg afterFirstFroms

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False
