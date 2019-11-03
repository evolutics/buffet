module Buffet.Build.ScheduleParallelInstructions
  ( get
  ) where

import qualified Buffet.Build.JoinConsecutiveRunInstructions as JoinConsecutiveRunInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude
  ( Bool(False, True)
  , (.)
  , (<>)
  , (==)
  , all
  , concatMap
  , fmap
  , fst
  , id
  , not
  , null
  , pure
  , snd
  , span
  )

get :: Ir.Buffet -> [Ir.DockerfilePart] -> [Ir.DockerfilePart]
get buffet =
  if Ir.optimize buffet
    then optimizedSchedule
    else unoptimizedSchedule

optimizedSchedule :: [Ir.DockerfilePart] -> [Ir.DockerfilePart]
optimizedSchedule =
  pure . JoinConsecutiveRunInstructions.get . scheduleWithSpannedRuns

scheduleWithSpannedRuns :: [Ir.DockerfilePart] -> Ir.DockerfilePart
scheduleWithSpannedRuns = scheduleNextPhase False []
  where
    scheduleNextPhase phase schedule queues =
      if all null queues
        then schedule
        else scheduleNextPhase phase' schedule' queues'
      where
        phase' = not phase
        schedule' = schedule <> concatMap fst spans
        spans = fmap (span (\instruction -> isRun instruction == phase)) queues
        queues' = fmap snd spans

isRun :: Docker.Instruction T.Text -> Bool
isRun (Docker.Run _) = True
isRun _ = False

unoptimizedSchedule :: [Ir.DockerfilePart] -> [Ir.DockerfilePart]
unoptimizedSchedule = id
