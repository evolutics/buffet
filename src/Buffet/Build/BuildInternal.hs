module Buffet.Build.BuildInternal
  ( get
  ) where

import qualified Buffet.Build.BeforeFirstBuildStage as BeforeFirstBuildStage
import qualified Buffet.Build.GlobalBuildStage as GlobalBuildStage
import qualified Buffet.Build.LocalBuildStages as LocalBuildStages
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import Prelude (($), (.), concat, fmap, mconcat)

get :: Ir.Buffet -> T.Text
get buffet =
  printDockerfileParts $
  concat
    [ BeforeFirstBuildStage.get buffet
    , LocalBuildStages.get buffet
    , GlobalBuildStage.get buffet
    ]

printDockerfileParts :: [Ir.DockerfilePart] -> T.Text
printDockerfileParts = TextTools.intercalateNewline . fmap printInstructions

printInstructions :: Ir.DockerfilePart -> T.Text
printInstructions = mconcat . fmap DockerTools.printInstruction
