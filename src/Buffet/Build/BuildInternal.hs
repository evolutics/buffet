module Buffet.Build.BuildInternal
  ( get
  ) where

import qualified Buffet.Build.ArgInstructions as ArgInstructions
import qualified Buffet.Build.BuildTools as BuildTools
import qualified Buffet.Build.GlobalBuildStage as GlobalBuildStage
import qualified Buffet.Build.LocalBuildStages as LocalBuildStages
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (($), concat)

get :: Ir.Buffet -> T.Text
get buffet =
  BuildTools.printDockerfileParts $
  concat
    [ ArgInstructions.get buffet
    , LocalBuildStages.get buffet
    , GlobalBuildStage.get buffet
    ]
