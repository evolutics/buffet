module Buffet.Parse.ParseBaseImage
  ( get
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Maybe(Just, Nothing), ($), (.), (<>), maybe, mconcat, mempty)

get :: Docker.Dockerfile -> T.Text
get = maybe mempty imageIdentifier . firstBaseImage

imageIdentifier :: Docker.BaseImage -> T.Text
imageIdentifier baseImage =
  mconcat
    [ printRegistry $ Docker.registryName image
    , Docker.imageName image
    , printTag $ Docker.tag baseImage
    , printDigest $ Docker.digest baseImage
    ]
  where
    printRegistry =
      maybe mempty $ \registry -> Docker.unRegistry registry <> T.singleton '/'
    image = Docker.image baseImage
    printTag = maybe mempty $ \tag -> T.singleton ':' <> Docker.unTag tag
    printDigest =
      maybe mempty $ \digest -> T.singleton '@' <> Docker.unDigest digest

firstBaseImage :: Docker.Dockerfile -> Maybe Docker.BaseImage
firstBaseImage = Maybe.listToMaybe . Maybe.mapMaybe maybeBaseImage
  where
    maybeBaseImage (Docker.InstructionPos (Docker.From baseImage) _ _) =
      Just baseImage
    maybeBaseImage _ = Nothing
