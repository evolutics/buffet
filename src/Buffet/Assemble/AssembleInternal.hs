module Buffet.Assemble.AssembleInternal
  ( get
  ) where

import qualified Buffet.Assemble.BeforeFirstBuildStage as BeforeFirstBuildStage
import qualified Buffet.Assemble.GlobalBuildStage as GlobalBuildStage
import qualified Buffet.Assemble.LocalBuildStages as LocalBuildStages
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import Prelude (($), (.), fmap, mconcat)

get :: Ir.Buffet -> T.Text
get buffet =
  printDockerfileParts $
  mconcat
    [ BeforeFirstBuildStage.get buffet
    , LocalBuildStages.get buffet
    , GlobalBuildStage.get buffet
    ]

printDockerfileParts :: [Ir.DockerfilePart] -> T.Text
printDockerfileParts = TextTools.intercalateNewline . fmap printInstructions

printInstructions :: Ir.DockerfilePart -> T.Text
printInstructions = mconcat . fmap DockerTools.printInstruction
