{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Comic (Comic(..)) where

import BasicPrelude hiding (id)

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

data Comic = Comic
  { id :: Int
  , title :: Text
  , description :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Comic
instance ToJSON Comic
