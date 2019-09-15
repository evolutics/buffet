module Buffet.Document.DefaultTemplate
  ( get
  ) where

import qualified Data.Text as T
import Prelude (String, ($), (.), either, error, id, show)
import qualified Text.Mustache as Mustache

get :: Mustache.Template
get = either (error . show) id $ Mustache.compileTemplate name source

name :: String
name = ""

source :: T.Text
source =
  T.unlines
    [ T.pack "# Buffet"
    , T.pack "{{#dishes}}"
    , T.pack ""
    , T.pack "## `{{option}}`"
    , T.pack ""
    , T.pack "[{{title}}]({{url}})"
    , T.pack ""
    , T.pack "{{#tags}}"
    , T.pack "- `{{key}}`"
    , T.pack "{{#values}}"
    , T.pack "  - {{.}}"
    , T.pack "{{/values}}"
    , T.pack "{{/tags}}"
    , T.pack "{{/dishes}}"
    ]
