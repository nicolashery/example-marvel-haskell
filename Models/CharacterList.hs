{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.CharacterList (CharacterList(..)) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Models.CharacterSummary (CharacterSummary)

data CharacterList = CharacterList
  { available :: Int
  , items :: [CharacterSummary]
  } deriving (Show, Generic)

instance FromJSON CharacterList
instance ToJSON CharacterList
