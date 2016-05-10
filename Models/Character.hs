{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Character
  ( Character(..)
  , getMarvelUrl
  , getNonEmptyDescription
  ) where

import BasicPrelude hiding (id)

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.Image (Image)
import Models.Url (Url)
import qualified Models.Url as U

data Character = Character
  { id :: Int
  , name :: Text
  , description :: Maybe Text
  , urls :: [Url]
  , thumbnail :: Image
  } deriving (Show, Generic)

instance FromJSON Character
instance ToJSON Character

getMarvelUrl :: Character -> Text
getMarvelUrl character =
  let defaultUrl = "http://marvel.com"
      maybeUrl = find isDetailUrl (urls character)
  in case maybeUrl of
    Nothing -> defaultUrl
    Just detailUrl -> U._url detailUrl

isDetailUrl :: Url -> Bool
isDetailUrl url = U._type url == "detail"

getNonEmptyDescription :: Character -> Maybe Text
getNonEmptyDescription character =
  case description character of
    Nothing -> Nothing
    Just "" -> Nothing
    Just desc -> Just desc
