module Buffet.Build.ConditionInstructionsInContext
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.HasArgInstructionWithName as HasArgInstructionWithName
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (Bool(False), ($), (.), any, id, maybe, mconcat, pure)

get :: Ir.Buffet -> Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
get buffet option =
  if hasOptionArgInstruction buffet option
    then ConditionInstructions.get configuration
    else id
  where
    configuration =
      ConditionInstructions.Configuration
        { ConditionInstructions.copyDummySourcePath =
            Ir.copyDummySourcePath buffet
        , ConditionInstructions.option = option
        }

hasOptionArgInstruction :: Ir.Buffet -> Ir.Option -> Bool
hasOptionArgInstruction buffet option =
  maybe False hasOptionArg . Map.lookup option $ Ir.optionToDish buffet
  where
    hasOptionArg = any (HasArgInstructionWithName.get option) . parts
    parts dish =
      mconcat
        [ pure $ Ir.beforeFirstBuildStage dish
        , Ir.localBuildStages dish
        , pure $ Ir.globalBuildStage dish
        ]
