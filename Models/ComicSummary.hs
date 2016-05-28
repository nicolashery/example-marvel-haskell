{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.ComicSummary
  ( ComicSummary(..)
  , getId
  , testGetId
  ) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Regex.PCRE ((=~))

import Models.Ids (ComicId)

data ComicSummary = ComicSummary
  { resourceURI :: Text
  , name :: Text
  } deriving (Show, Generic)

instance FromJSON ComicSummary
instance ToJSON ComicSummary

getId :: ComicSummary -> Maybe ComicId
getId comicSummary =
  -- Note: need to use String, can't use Text with Regex apparently
  let uri = T.unpack (resourceURI comicSummary)
      regex = "\\/comics\\/([0-9]+)" :: String
      -- Will return something like `[["/comics/21366","21366"]]`
      result = uri =~ regex :: [[String]]
  in case result of
    ((_:x:_):_) -> readMay (T.pack x) :: Maybe Int
    _ -> Nothing

testGetId :: Maybe ComicId
testGetId =
  let comicSummary = ComicSummary
        { resourceURI="http://gateway.marvel.com/v1/public/comics/21366"
        , name="Avengers: The Initiative (2007) #14"
        }
  in getId comicSummary
