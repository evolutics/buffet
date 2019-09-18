module Buffet.Test.TestInternal
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Prelude
  ( IO
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

get :: Ir.Buffet -> Map.Map Ir.Option T.Text -> IO ()
get buffetIr arguments = do
  let buffet = BuildInternal.get buffetIr
  imageId <- dockerBuild buffet arguments
  let optionToDish = filterTestedDishes (Ir.optionToDish buffetIr) arguments
      tests =
        Map.mapWithKey
          (\option dish ->
             case Ir.testCommand dish of
               Nothing ->
                 putStderrLine $
                 mconcat [T.pack "No test for dish: ", Ir.option option]
               Just testCommand ->
                 Process.runProcess_ $
                 Process.proc
                   "docker"
                   ["run", T.unpack imageId, "sh", "-c", T.unpack testCommand])
          optionToDish
  sequence_ tests

dockerBuild :: T.Text -> Map.Map Ir.Option T.Text -> IO T.Text
dockerBuild dockerfile arguments = do
  rawImageIdLine <-
    Process.readProcessStdout_ $
    Process.setStdin (textInput dockerfile) processBase
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
           ]) $
      Map.toAscList arguments

filterTestedDishes ::
     Map.Map Ir.Option Ir.Dish
  -> Map.Map Ir.Option T.Text
  -> Map.Map Ir.Option Ir.Dish
filterTestedDishes optionToDish arguments =
  Map.restrictKeys optionToDish relevantArgumentOptions
  where
    relevantArgumentOptions = Map.keysSet $ Map.filter (not . T.null) arguments

putStderrLine :: T.Text -> IO ()
putStderrLine = T.IO.hPutStrLn IO.stderr
