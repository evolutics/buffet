module UtilityEntries.Git
  ( get
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Prelude (($), (.))
import qualified Tags.Help as Help
import qualified Utilities

get :: Utilities.Entry
get =
  Utilities.Entry
    { Utilities.option = T.pack "git"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.installation =
              Utilities.Command
                { Utilities.indentableLines =
                    [T.pack "apk add --no-cache \"git==${git}\" \\"]
                }
          , Utilities.extraOptionsWithDefaults = Map.empty
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "Git"
                , Utilities.link = T.pack "https://git-scm.com"
                , Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "git --help"
                }
          }
    }
