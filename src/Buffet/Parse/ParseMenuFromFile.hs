{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Parse.ParseMenuFromFile
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Menu as Menu
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified GHC.Generics as Generics
import Prelude
  ( Eq
  , FilePath
  , IO
  , Maybe
  , Ord
  , Show
  , ($)
  , fmap
  , maybe
  , mconcat
  , pure
  , show
  )
import qualified System.FilePath as FilePath

data Exception =
  Exception FilePath Yaml.ParseException

instance Show Exception where
  show (Exception path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

data RawMenu =
  RawMenu
    { copyDummySourcePath :: Maybe T.Text
    , optionToDish :: Maybe (Map.Map Ir.Option FilePath)
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Yaml.FromJSON RawMenu where
  parseJSON = Aeson.genericParseJSON TextTools.defaultJsonOptions

get :: FilePath -> IO Menu.Menu
get menu = do
  raw <- getRaw menu
  pure
    Menu.Menu
      { Menu.copyDummySourcePath =
          Maybe.fromMaybe (Menu.copyDummySourcePath Menu.defaultMenu) $
          copyDummySourcePath raw
      , Menu.optionToDish =
          maybe
            (Menu.optionToDish Menu.defaultMenu)
            (fmap $ FilePath.combine folder) $
          optionToDish raw
      }
  where
    folder = FilePath.takeDirectory menu

getRaw :: FilePath -> IO RawMenu
getRaw menu =
  ExceptionTools.eitherThrow (Exception menu) $ Yaml.decodeFileEither menu
