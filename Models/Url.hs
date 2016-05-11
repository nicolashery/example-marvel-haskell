{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Url
  ( Url(..)
  , isDetailUrl
  ) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)

data Url = Url
  { _type :: Text
  , _url :: Text
  } deriving (Show, Generic)

instance ToJSON Url where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON Url where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

isDetailUrl :: Url -> Bool
isDetailUrl url = _type url == "detail"
