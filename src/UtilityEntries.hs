module UtilityEntries
  ( get
  ) where

import Prelude ()
import qualified Utilities
import qualified UtilityEntries.Brittany as Brittany
import qualified UtilityEntries.Git as Git
import qualified UtilityEntries.Gitlint as Gitlint
import qualified UtilityEntries.Hindent as Hindent
import qualified UtilityEntries.Hlint as Hlint
import qualified UtilityEntries.Hunspell as Hunspell
import qualified UtilityEntries.Prettier as Prettier

get :: [Utilities.Entry]
get =
  [ Brittany.get
  , Git.get
  , Gitlint.get
  , Hindent.get
  , Hlint.get
  , Hunspell.get
  , Prettier.get
  ]
