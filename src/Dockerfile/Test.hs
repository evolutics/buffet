module Dockerfile.Test
  ( get
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.Yaml as Yaml
import qualified Dockerfile.BuildInternal as BuildInternal
import qualified Dockerfile.Ir as Ir
import qualified Dockerfile.Parse as Parse
import Prelude
  ( FilePath
  , IO
  , Maybe(Just, Nothing)
  , ($)
  , (.)
  , concatMap
  , mconcat
  , not
  , return
  , sequence_
  )
import qualified System.IO as IO
import qualified System.Process.Typed as Process

get :: FilePath -> FilePath -> IO ()
get source argumentsFile = do
  box <- Parse.get source
  let dockerfile = BuildInternal.get box
  arguments <- Yaml.decodeFileThrow argumentsFile
  let _ = arguments :: Map.Map T.Text T.Text
  imageId <- dockerBuild dockerfile arguments
  let optionToUtility = filterTestedUtilities (Ir.optionToUtility box) arguments
      tests =
        Map.mapWithKey
          (\option utility ->
             case Ir.testCommand utility of
               Nothing ->
                 putStderrLine $
                 mconcat [T.pack "No test for utility: ", option]
               Just testCommand ->
                 Process.runProcess_ $
                 Process.proc
                   "docker"
                   ["run", T.unpack imageId, "sh", "-c", T.unpack testCommand])
          optionToUtility
  sequence_ tests

dockerBuild :: T.Text -> Map.Map T.Text T.Text -> IO T.Text
dockerBuild dockerfile arguments = do
  rawImageIdLine <-
    Process.readProcessStdout_ $
    Process.setStdin (textInput dockerfile) processBase
  let imageIdLine = Lazy.toStrict $ Encoding.decodeUtf8 rawImageIdLine
      imageId = T.stripEnd imageIdLine
  return imageId
  where
    textInput = Process.byteStringInput . Encoding.encodeUtf8 . Lazy.fromStrict
    processBase =
      Process.proc "docker" $ mconcat [["build", "--quiet"], buildArgs, ["-"]]
    buildArgs =
      concatMap
        (\(key, value) ->
           ["--build-arg", mconcat [T.unpack key, "=", T.unpack value]]) $
      Map.toAscList arguments

filterTestedUtilities ::
     Map.Map T.Text Ir.Utility
  -> Map.Map T.Text T.Text
  -> Map.Map T.Text Ir.Utility
filterTestedUtilities optionToUtility arguments =
  Map.restrictKeys optionToUtility relevantArgumentOptions
  where
    relevantArgumentOptions = Map.keysSet $ Map.filter (not . T.null) arguments

putStderrLine :: T.Text -> IO ()
putStderrLine = T.IO.hPutStrLn IO.stderr
