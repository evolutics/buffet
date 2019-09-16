module Buffet
  ( main
  ) where

import qualified Buffet.Facade as Facade
import qualified Control.Monad as Monad
import qualified Options.Applicative as Options
import Prelude (IO, ($), (<$>), (<*>), mconcat, mempty)

main :: IO ()
main = Monad.join $ Options.execParser (Options.info commands mempty)

commands :: Options.Parser (IO ())
commands =
  Options.hsubparser $
  mconcat
    [ Options.command
        "build"
        (Options.info
           (Facade.build <$>
            Options.argument Options.str (Options.metavar "SOURCE"))
           mempty)
    , Options.command
        "document"
        (Options.info
           (Facade.document <$>
            Options.argument Options.str (Options.metavar "SOURCE"))
           mempty)
    , Options.command
        "parse"
        (Options.info
           (Facade.parse <$>
            Options.argument Options.str (Options.metavar "SOURCE"))
           mempty)
    , Options.command
        "test"
        (Options.info
           (Facade.test <$>
            Options.argument Options.str (Options.metavar "SOURCE") <*>
            Options.argument Options.str (Options.metavar "ARGUMENTS"))
           mempty)
    ]
