{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.FeaturedCharacter (FeaturedCharacter(..)) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.Ids (CharacterId)
import Models.Image (Image)

data FeaturedCharacter = FeaturedCharacter
  { id :: CharacterId
  , name :: Text
  , thumbnail :: Image
  } deriving (Show, Generic)

instance FromJSON FeaturedCharacter
instance ToJSON FeaturedCharacter
