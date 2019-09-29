module Buffet.Test.UsingDockerImage
  ( Configuration(..)
  , DockerBuild(..)
  , get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Eq, IO, Ord, Show, ($), (.), concatMap, mconcat, pure)
import qualified System.IO as IO
import qualified System.Process.Typed as Process

data Configuration =
  Configuration
    { log :: IO.Handle
    , dockerBuild :: DockerBuild
    }
  deriving (Eq, Show)

data DockerBuild =
  DockerBuild
    { dockerfile :: T.Text
    , arguments :: Map.Map Ir.Option T.Text
    }
  deriving (Eq, Ord, Show)

get :: (T.Text -> IO a) -> Configuration -> IO a
get useImage configuration =
  Exception.bracket
    (buildImage configuration)
    (removeImage configuration)
    useImage

buildImage :: Configuration -> IO T.Text
buildImage configuration = do
  rawImageIdLine <-
    Process.readProcessStdout_ . Process.setStderr (Process.useHandleOpen log') $
    Process.setStdin (textInput dockerfile') processBase
  let imageIdLine = TextTools.decodeUtf8 rawImageIdLine
      imageId = T.stripEnd imageIdLine
  pure imageId
  where
    log' = log configuration
    textInput = Process.byteStringInput . TextTools.encodeUtf8
    dockerfile' = dockerfile $ dockerBuild configuration
    processBase =
      Process.proc "docker" $ mconcat [["build", "--quiet"], buildArgs, ["-"]]
    buildArgs =
      concatMap
        (\(key, value) ->
           [ "--build-arg"
           , mconcat [T.unpack $ Ir.option key, "=", T.unpack value]
           ]) $
      Map.toAscList arguments'
    arguments' = arguments $ dockerBuild configuration

removeImage :: Configuration -> T.Text -> IO ()
removeImage configuration imageId =
  Process.runProcess_ .
  Process.setStderr (Process.useHandleOpen log') .
  Process.setStdout (Process.useHandleOpen log') $
  Process.proc "docker" ["rmi", T.unpack imageId]
  where
    log' = log configuration
