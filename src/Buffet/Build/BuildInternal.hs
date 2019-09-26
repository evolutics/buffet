module Buffet.Build.BuildInternal
  ( get
  ) where

import qualified Buffet.Build.ArgInstructions as ArgInstructions
import qualified Buffet.Build.Configuration as Configuration
import qualified Buffet.Build.GlobalBuildStage as GlobalBuildStage
import qualified Buffet.Build.LocalBuildStages as LocalBuildStages
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Bool(True), ($), (.), concat, fmap, mconcat)

get :: Ir.Buffet -> T.Text
get buffet =
  printDockerfileParts $
  concat
    [ ArgInstructions.get configuration buffet
    , LocalBuildStages.get configuration buffet
    , GlobalBuildStage.get configuration buffet
    ]

configuration :: Configuration.Configuration
configuration =
  Configuration.Configuration
    { Configuration.baseImageName = T.pack "alpine"
    , Configuration.baseImageTagOption = Ir.Option $ T.pack "alpine_version"
    , Configuration.baseImageTagValue = T.pack "3.9.4"
    , Configuration.workdir = "/workdir"
    , Configuration.optimize = True
    }

printDockerfileParts :: [Ir.DockerfilePart] -> T.Text
printDockerfileParts = TextTools.intercalateNewline . fmap printInstructions

printInstructions :: Ir.DockerfilePart -> T.Text
printInstructions = mconcat . fmap printInstruction
  where
    printInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      T.unlines [mconcat [T.pack "RUN ", command]]
    printInstruction instruction =
      Lazy.toStrict $ Docker.prettyPrint [Docker.instructionPos instruction]
