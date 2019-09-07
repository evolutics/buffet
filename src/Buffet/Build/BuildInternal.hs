module Buffet.Build.BuildInternal
  ( get
  ) where

import qualified Buffet.Build.ArgInstructions as ArgInstructions
import qualified Buffet.Build.GlobalBuildStage as GlobalBuildStage
import qualified Buffet.Build.LocalBuildStages as LocalBuildStages
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), (.), concat, fmap)

get :: Ir.Buffet -> T.Text
get buffet =
  printDockerfileParts $
  concat
    [ ArgInstructions.get buffet
    , LocalBuildStages.get buffet
    , GlobalBuildStage.get buffet
    ]

printDockerfileParts :: [Ir.DockerfilePart] -> T.Text
printDockerfileParts = TextTools.intercalateNewline . fmap printInstructions

printInstructions :: Ir.DockerfilePart -> T.Text
printInstructions = T.concat . fmap printInstruction
  where
    printInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      T.unlines [T.concat [T.pack "RUN ", command]]
    printInstruction instruction =
      Lazy.toStrict $ Docker.prettyPrint [Docker.instructionPos instruction]
