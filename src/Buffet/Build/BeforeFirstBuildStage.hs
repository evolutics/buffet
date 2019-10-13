module Buffet.Build.BeforeFirstBuildStage
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude (Maybe(Just), ($), (.), fmap, pure, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = pure . Set.toAscList . dishesInstructions

dishesInstructions :: Ir.Buffet -> Set.Set (Docker.Instruction T.Text)
dishesInstructions =
  Set.unions . fmap (uncurry dishInstructions) . Map.toList . Ir.optionToDish

dishInstructions :: Ir.Option -> Ir.Dish -> Set.Set (Docker.Instruction T.Text)
dishInstructions option dish =
  Set.insert optionArg $ Set.fromList givenInstructions
  where
    optionArg = Docker.Arg (Ir.option option) (Just $ T.pack "''")
    givenInstructions = Ir.beforeFirstBuildStage $ Ir.instructionPartition dish
