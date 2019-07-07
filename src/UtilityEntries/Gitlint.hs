module UtilityEntries.Gitlint
  ( get
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Prelude ()
import qualified Utilities

get :: Utilities.Entry
get =
  Utilities.Entry
    { Utilities.option = T.pack "gitlint"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.installation =
              Utilities.Command
                { Utilities.indentableLines =
                    [ T.pack "apk add --no-cache git python3 \\"
                    , T.pack "&& pip3 install \"gitlint==${gitlint}\" \\"
                    ]
                }
          , Utilities.extraOptionsWithDefaults = Map.empty
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "Gitlint"
                , Utilities.link =
                    T.pack "http://jorisroovers.github.io/gitlint"
                , Utilities.tags = Set.empty
                , Utilities.help =
                    Utilities.Command
                      {Utilities.indentableLines = [T.pack "gitlint --help"]}
                }
          }
    }