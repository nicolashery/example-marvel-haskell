{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Comic
  ( Comic(..)
  , ComicId
  , getMarvelUrl
  , getNonEmptyDescription
  , hasCharacters
  , getCharacters
  ) where

import BasicPrelude hiding (id)

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.CharacterList (CharacterList)
import qualified Models.CharacterList as CL
import Models.CharacterSummary (CharacterSummary)
import Models.Ids (ComicId)
import Models.Image (Image)
import Models.Url (Url, isDetailUrl)
import qualified Models.Url as U

data Comic = Comic
  { id :: ComicId
  , title :: Text
  , description :: Maybe Text
  , urls :: [Url]
  , thumbnail :: Image
  , characters :: CharacterList
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

hasCharacters :: Comic -> Bool
hasCharacters comic = CL.available (characters comic) > 0

getCharacters :: Comic -> [CharacterSummary]
getCharacters comic = CL.items (characters comic)
