module Buffet.Build.ConditionInstructionsInContext
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Ir.Ir as Ir
import Prelude ()

get :: Ir.Buffet -> Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
get buffet option = ConditionInstructions.get configuration
  where
    configuration =
      ConditionInstructions.Configuration
        { ConditionInstructions.copyDummySourcePath =
            Ir.copyDummySourcePath buffet
        , ConditionInstructions.option = option
        }
