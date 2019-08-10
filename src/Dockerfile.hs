module Dockerfile
  ( get
  ) where

import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Prelude
  ( Bool
  , Ordering
  , ($)
  , (.)
  , (<>)
  , compare
  , concat
  , const
  , fmap
  , fst
  , uncurry
  )
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
argInstructions box =
  intercalateBlankLines $
  fmap orderedArgInstructions [publicOptions, privateOptions]
  where
    orderedArgInstructions = fmap argInstruction . orderOptionMap
    argInstruction (key, value) =
      T.concat [T.pack "ARG ", key, T.pack "=", value]
    (privateOptions, publicOptions) =
      Map.partitionWithKey (\key _ -> isPrivateOption key) options
    options = Map.unions [mainOptions, baseImageOptions, extraOptions]
    mainOptions = fmap (const $ T.pack "''") optionToUtility
    optionToUtility = Utilities.optionToUtility box
    baseImageOptions =
      Map.singleton (T.pack "_alpine_version") $ T.pack "'3.9.4'"
    extraOptions =
      Map.unions $ fmap Utilities.extraOptionsWithDefaults utilities
    utilities = Map.elems optionToUtility

orderOptionMap :: Map.Map T.Text a -> [(T.Text, a)]
orderOptionMap = List.sortBy (Function.on compareOptions fst) . Map.toList

compareOptions :: T.Text -> T.Text -> Ordering
compareOptions = Function.on compare key
  where
    key option = (isPrivateOption option, option)

isPrivateOption :: T.Text -> Bool
isPrivateOption = T.isPrefixOf $ T.pack "_"

utilityBuildStages :: Utilities.Box -> [T.Text]
utilityBuildStages box =
  intercalateBlankLines . fmap (uncurry utilityBuildStage) $
  orderOptionMap optionToUtility
  where
    optionToUtility = Utilities.optionToUtility box

utilityBuildStage :: T.Text -> Utilities.Utility -> [T.Text]
utilityBuildStage option utility =
  concat
    [ [T.concat [T.pack "FROM ", baseImage, T.pack " AS ", option]]
    , fmap (T.pack "ARG " <>) orderedOptions
    , runInstruction option utility
    ]
  where
    orderedOptions = orderOptionSet options
    options = Set.insert option extraOptions
    extraOptions = Map.keysSet $ Utilities.extraOptionsWithDefaults utility

baseImage :: T.Text
baseImage = T.pack "alpine:\"${_alpine_version}\""

orderOptionSet :: Set.Set T.Text -> [T.Text]
orderOptionSet = List.sortBy compareOptions . Set.toList

runInstruction :: T.Text -> Utilities.Utility -> [T.Text]
runInstruction option utility =
  concat
    [ [T.concat [T.pack "RUN if [[ -n \"${", option, T.pack "}\" ]]; then \\"]]
    , indentLines . indentLines $ T.lines dockerfile
    , [indentLine $ T.pack "; fi"]
    ]
  where
    indentLines :: [T.Text] -> [T.Text]
    indentLines = fmap indentLine
    indentLine = T.append $ T.pack "  "
    dockerfile = Utilities.dockerfile utility

workdirInstruction :: [T.Text]
workdirInstruction = [T.pack "WORKDIR /workdir"]

copyInstructions :: Utilities.Box -> [T.Text]
copyInstructions box = fmap copyInstruction options
  where
    copyInstruction option =
      T.concat [T.pack "COPY --from=", option, T.pack " / /"]
    options = orderOptionSet . Map.keysSet $ Utilities.optionToUtility box
