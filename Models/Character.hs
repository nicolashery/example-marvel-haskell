{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Character (Character(..)) where

import BasicPrelude hiding (id)

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

data Character = Character
  { id :: Int
  , name :: Text
  , description :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Character
instance ToJSON Character
