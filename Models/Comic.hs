{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Comic
  ( Comic(..)
  , getMarvelUrl
  , getNonEmptyDescription
  ) where

import BasicPrelude hiding (id)

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.Image (Image)
import Models.Url (Url, isDetailUrl)
import qualified Models.Url as U

data Comic = Comic
  { id :: Int
  , title :: Text
  , description :: Maybe Text
  , urls :: [Url]
  , thumbnail :: Image
  } deriving (Show, Generic)

instance FromJSON Comic
instance ToJSON Comic

getMarvelUrl :: Comic -> Text
getMarvelUrl comic =
  let defaultUrl = "http://marvel.com"
      maybeUrl = find isDetailUrl (urls comic)
  in case maybeUrl of
    Nothing -> defaultUrl
    Just detailUrl -> U._url detailUrl

getNonEmptyDescription :: Comic -> Maybe Text
getNonEmptyDescription comic =
  case description comic of
    Nothing -> Nothing
    Just "" -> Nothing
    Just desc -> Just desc
