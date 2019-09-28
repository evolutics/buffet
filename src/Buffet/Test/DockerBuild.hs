module Buffet.Test.DockerBuild
  ( DockerBuild(..)
  , get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Eq, IO, Ord, Show, ($), (.), concatMap, mconcat, pure)
import qualified System.Process.Typed as Process

data DockerBuild =
  DockerBuild
    { dockerfile :: T.Text
    , arguments :: Map.Map Ir.Option T.Text
    }
  deriving (Eq, Ord, Show)

get :: DockerBuild -> IO T.Text
get build = do
  rawImageIdLine <-
    Process.readProcessStdout_ $
    Process.setStdin (textInput $ dockerfile build) processBase
  let imageIdLine = TextTools.decodeUtf8 rawImageIdLine
      imageId = T.stripEnd imageIdLine
  pure imageId
  where
    textInput = Process.byteStringInput . TextTools.encodeUtf8
    processBase =
      Process.proc "docker" $ mconcat [["build", "--quiet"], buildArgs, ["-"]]
    buildArgs =
      concatMap
        (\(key, value) ->
           [ "--build-arg"
           , mconcat [T.unpack $ Ir.option key, "=", T.unpack value]
           ]) .
      Map.toAscList $
      arguments build
