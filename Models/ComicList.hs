{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.ComicList (ComicList(..)) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.ComicSummary (ComicSummary)

data ComicList = ComicList
  { available :: Int
  , items :: [ComicSummary]
  } deriving (Show, Generic)

instance FromJSON ComicList
instance ToJSON ComicList
