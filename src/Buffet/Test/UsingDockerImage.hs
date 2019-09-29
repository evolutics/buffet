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
import qualified Data.Word as Word
import qualified Numeric
import Prelude (Eq, IO, Ord, Show, ($), (.), concatMap, mconcat, pure)
import qualified System.IO as IO
import qualified System.Process.Typed as Process
import qualified System.Random as Random

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
  image <- randomImage
  let processBase =
        Process.proc "docker" $
        mconcat [["build", "--tag", T.unpack image], buildArgs, ["-"]]
  Process.runProcess_ .
    Process.setStderr (Process.useHandleOpen log') .
    Process.setStdout (Process.useHandleOpen log') $
    Process.setStdin (textInput dockerfile') processBase
  pure image
  where
    buildArgs =
      concatMap
        (\(key, value) ->
           [ "--build-arg"
           , mconcat [T.unpack $ Ir.option key, "=", T.unpack value]
           ]) $
      Map.toAscList arguments'
    arguments' = arguments $ dockerBuild configuration
    log' = log configuration
    textInput = Process.byteStringInput . TextTools.encodeUtf8
    dockerfile' = dockerfile $ dockerBuild configuration

randomImage :: IO T.Text
randomImage = do
  tagNumber <- Random.randomIO
  let _ = tagNumber :: Word.Word64
      tag = T.pack $ Numeric.showHex tagNumber ""
  pure $ mconcat [name, T.pack ":", tag]
  where
    name = T.pack "buffet-tmp"

removeImage :: Configuration -> T.Text -> IO ()
removeImage configuration image =
  Process.runProcess_ .
  Process.setStderr (Process.useHandleOpen log') .
  Process.setStdout (Process.useHandleOpen log') $
  Process.proc "docker" ["rmi", T.unpack image]
  where
    log' = log configuration
