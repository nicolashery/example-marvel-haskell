{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.FeaturedComic (FeaturedComic(..)) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.Image (Image)

data FeaturedComic = FeaturedComic
  { id :: Int
  , name :: Text
  , thumbnail :: Image
  } deriving (Show, Generic)

instance FromJSON FeaturedComic
instance ToJSON FeaturedComic
