{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Image
  ( Image(..)
  , getPortraitXLarge
  , getStandardXLarge
  ) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

data Image = Image
  { path :: Text
  , extension :: Text
  } deriving (Show, Generic)

instance FromJSON Image
instance ToJSON Image

getPortraitXLarge :: Image -> Text
getPortraitXLarge image = path image ++ "/portrait_xlarge." ++ extension image

getStandardXLarge :: Image -> Text
getStandardXLarge image = path image ++ "/standard_xlarge." ++ extension image
