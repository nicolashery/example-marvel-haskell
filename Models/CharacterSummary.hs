{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.CharacterSummary
  ( CharacterSummary(..)
  , getId
  , testGetId
  ) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Regex.PCRE ((=~))

data CharacterSummary = CharacterSummary
  { resourceURI :: Text
  , name :: Text
  } deriving (Show, Generic)

instance FromJSON CharacterSummary
instance ToJSON CharacterSummary

getId :: CharacterSummary -> Maybe Int
getId characterSummary =
  -- Note: need to use String, can't use Text with Regex apparently
  let uri = T.unpack (resourceURI characterSummary)
      regex = "\\/characters\\/([0-9]+)" :: String
      -- Will return something like `[["/characters/1011334","1011334"]]`
      result = uri =~ regex :: [[String]]
  in case result of
    ((_:x:_):_) -> readMay (T.pack x) :: Maybe Int
    _ -> Nothing

testGetId :: Maybe Int
testGetId =
  let characterSummary = CharacterSummary
        { resourceURI="http://gateway.marvel.com/v1/public/characters/1011334"
        , name="3-D Man"
        }
  in getId characterSummary
