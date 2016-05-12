{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Character
  ( Character(..)
  , getMarvelUrl
  , getNonEmptyDescription
  , hasComics
  , getComics
  ) where

import BasicPrelude hiding (id)

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.ComicList (ComicList)
import qualified Models.ComicList as CL
import Models.ComicSummary (ComicSummary)
import Models.Image (Image)
import Models.Url (Url, isDetailUrl)
import qualified Models.Url as U

data Character = Character
  { id :: Int
  , name :: Text
  , description :: Maybe Text
  , urls :: [Url]
  , thumbnail :: Image
  , comics :: ComicList
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

getNonEmptyDescription :: Character -> Maybe Text
getNonEmptyDescription character =
  case description character of
    Nothing -> Nothing
    Just "" -> Nothing
    Just desc -> Just desc

hasComics :: Character -> Bool
hasComics character = CL.available (comics character) > 0

getComics :: Character -> [ComicSummary]
getComics character = CL.items (comics character)
