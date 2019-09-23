module Buffet.Toolbox.ExceptionTools
  ( sequenceAccumulatingExceptions
  ) where

import qualified Control.Exception as Exception
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Prelude
  ( IO
  , Show
  , Traversable
  , ($)
  , (.)
  , fmap
  , maybe
  , pure
  , show
  , traverse
  , undefined
  , unlines
  )

newtype ExceptionList =
  ExceptionList (NonEmpty.NonEmpty Exception.SomeException)

instance Show ExceptionList where
  show (ExceptionList exceptions) =
    unlines . NonEmpty.toList $ fmap show exceptions

instance Exception.Exception ExceptionList

sequenceAccumulatingExceptions :: Traversable t => t (IO a) -> IO (t a)
sequenceAccumulatingExceptions actions = do
  results <- traverse Exception.try actions
  let successes = fmap (Either.fromRight undefined) results
      failures = Either.lefts $ Foldable.toList results
  maybe (pure successes) (Exception.throwIO . ExceptionList) $
    NonEmpty.nonEmpty failures
