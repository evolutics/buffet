module Buffet.Build.ScheduleParallelInstructions
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude
  ( Bool(False, True)
  , ($)
  , (.)
  , (<>)
  , (==)
  , all
  , concatMap
  , fmap
  , foldr
  , fst
  , mconcat
  , not
  , null
  , snd
  , span
  )

get :: [Ir.DockerfilePart] -> Ir.DockerfilePart
get = mergeConsecutiveRuns . scheduleWithSpannedRuns

mergeConsecutiveRuns :: Ir.DockerfilePart -> Ir.DockerfilePart
mergeConsecutiveRuns = foldr process []
  where
    process (Docker.Run first) (Docker.Run second:rest) =
      Docker.Run (mergeRuns first second) : rest
    process first rest = first : rest

mergeRuns ::
     Docker.Arguments T.Text
  -> Docker.Arguments T.Text
  -> Docker.Arguments T.Text
mergeRuns first second =
  Syntax.ArgumentsText $
  mconcat [command first, T.pack " \\\n  && ", command second]
  where
    command (Syntax.ArgumentsText shell) = shell
    command (Syntax.ArgumentsList exec) = exec

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
