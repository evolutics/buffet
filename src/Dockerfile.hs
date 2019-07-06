module Dockerfile
  ( get
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (($), (.), (<>), concat, const, fmap, uncurry)
import qualified Utilities

get :: Utilities.Box -> T.Text
get box =
  T.unlines $
  intercalateBlankLines
    [ argInstructions box
    , utilityBuildStages box
    , [T.pack "FROM " <> baseImage]
    , workdirInstruction
    , copyInstructions box
    ]

intercalateBlankLines :: [[T.Text]] -> [T.Text]
intercalateBlankLines = List.intercalate [T.pack ""]

argInstructions :: Utilities.Box -> [T.Text]
argInstructions box = orderedArgInstructions options
  where
    orderedArgInstructions = fmap argInstruction . Map.toAscList
    argInstruction (key, value) =
      T.concat [T.pack "ARG ", key, T.pack "=", value]
    options = fmap (const $ T.pack "''") optionToUtility
    optionToUtility = Utilities.optionToUtility box

utilityBuildStages :: Utilities.Box -> [T.Text]
utilityBuildStages box =
  intercalateBlankLines . fmap (uncurry utilityBuildStage) $
  Map.toAscList optionToUtility
  where
    optionToUtility = Utilities.optionToUtility box

utilityBuildStage :: T.Text -> Utilities.Utility -> [T.Text]
utilityBuildStage option utility =
  concat
    [ [T.concat [T.pack "FROM ", baseImage, T.pack " AS ", option]]
    , [T.pack "ARG " <> option]
    , runInstruction option utility
    ]

baseImage :: T.Text
baseImage = T.pack "alpine:3.9.4"

runInstruction :: T.Text -> Utilities.Utility -> [T.Text]
runInstruction option utility =
  concat
    [ [T.concat [T.pack "RUN if [[ -n \"${", option, T.pack "}\" ]]; then \\"]]
    , indentLines $ indentLines indentableLines
    , [indentLine $ T.pack "; fi"]
    ]
  where
    indentLines :: [T.Text] -> [T.Text]
    indentLines = fmap indentLine
    indentLine = T.append $ T.pack "  "
    indentableLines = Utilities.indentableLines $ Utilities.installation utility

workdirInstruction :: [T.Text]
workdirInstruction = [T.pack "WORKDIR /workdir"]

copyInstructions :: Utilities.Box -> [T.Text]
copyInstructions box = fmap copyInstruction options
  where
    copyInstruction option =
      T.concat [T.pack "COPY --from=", option, T.pack " / /"]
    options = Map.keys $ Utilities.optionToUtility box
