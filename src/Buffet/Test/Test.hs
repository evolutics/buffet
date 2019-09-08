module Buffet.Test.Test
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Parse as Parse
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.Yaml as Yaml
import Prelude
  ( FilePath
  , IO
  , Maybe(Just, Nothing)
  , ($)
  , (.)
  , concatMap
  , mconcat
  , not
  , pure
  , sequence_
  )
import qualified System.IO as IO
import qualified System.Process.Typed as Process

get :: FilePath -> FilePath -> IO ()
get buffetSource argumentsFile = do
  buffetIr <- Parse.get buffetSource
  let buffet = BuildInternal.get buffetIr
  arguments <- Yaml.decodeFileThrow argumentsFile
  let _ = arguments :: Map.Map T.Text T.Text
  imageId <- dockerBuild buffet arguments
  let optionToDish = filterTestedDishes (Ir.optionToDish buffetIr) arguments
      tests =
        Map.mapWithKey
          (\option dish ->
             case Ir.testCommand dish of
               Nothing ->
                 putStderrLine $ mconcat [T.pack "No test for dish: ", option]
               Just testCommand ->
                 Process.runProcess_ $
                 Process.proc
                   "docker"
                   ["run", T.unpack imageId, "sh", "-c", T.unpack testCommand])
          optionToDish
  sequence_ tests

dockerBuild :: T.Text -> Map.Map T.Text T.Text -> IO T.Text
dockerBuild dockerfile arguments = do
  rawImageIdLine <-
    Process.readProcessStdout_ $
    Process.setStdin (textInput dockerfile) processBase
  let imageIdLine = Lazy.toStrict $ Encoding.decodeUtf8 rawImageIdLine
      imageId = T.stripEnd imageIdLine
  pure imageId
  where
    textInput = Process.byteStringInput . Encoding.encodeUtf8 . Lazy.fromStrict
    processBase =
      Process.proc "docker" $ mconcat [["build", "--quiet"], buildArgs, ["-"]]
    buildArgs =
      concatMap
        (\(key, value) ->
           ["--build-arg", mconcat [T.unpack key, "=", T.unpack value]]) $
      Map.toAscList arguments

filterTestedDishes ::
     Map.Map T.Text Ir.Dish -> Map.Map T.Text T.Text -> Map.Map T.Text Ir.Dish
filterTestedDishes optionToDish arguments =
  Map.restrictKeys optionToDish relevantArgumentOptions
  where
    relevantArgumentOptions = Map.keysSet $ Map.filter (not . T.null) arguments

putStderrLine :: T.Text -> IO ()
putStderrLine = T.IO.hPutStrLn IO.stderr
