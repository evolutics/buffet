module Dockerfile.Tester
  ( get
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.Yaml as Yaml
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Parser as Parser
import qualified Dockerfile.Printer as Printer
import Prelude (FilePath, IO, ($), (.), concatMap, mapM_, mconcat, not, return)
import qualified System.Process.Typed as Process

get :: FilePath -> FilePath -> IO ()
get source argumentsFile = do
  box <- Parser.get source
  let dockerfile = Printer.get box
  arguments <- Yaml.decodeFileThrow argumentsFile
  let _ = arguments :: Map.Map T.Text T.Text
  imageId <- dockerBuild dockerfile arguments
  let optionToUtility =
        filterTestedUtilities (Intermediate.optionToUtility box) arguments
      tests =
        Map.mapWithKey
          (\option _ ->
             Process.proc
               "docker"
               ["run", T.unpack imageId, T.unpack option, "--help"])
          optionToUtility
  mapM_ Process.runProcess_ tests

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
     Map.Map T.Text Intermediate.Utility
  -> Map.Map T.Text T.Text
  -> Map.Map T.Text Intermediate.Utility
filterTestedUtilities optionToUtility arguments =
  Map.restrictKeys optionToUtility relevantArgumentOptions
  where
    relevantArgumentOptions = Map.keysSet $ Map.filter (not . T.null) arguments
