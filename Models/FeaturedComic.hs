{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.FeaturedComic (FeaturedComic(..)) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.Ids (ComicId)
import Models.Image (Image)

data FeaturedComic = FeaturedComic
  { id :: ComicId
  , name :: Text
  , thumbnail :: Image
  } deriving (Show, Generic)

instance FromJSON FeaturedComic
instance ToJSON FeaturedComic
