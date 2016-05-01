{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Pagination
  ( Pagination(..)
  , getStart
  , getEnd
  , isFirstPage
  , isLastPage
  , previousPageOffset
  , nextPageOffset
  ) where

import BasicPrelude

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

data Pagination = Pagination
  { offset :: Int
  , limit :: Int
  , total :: Int
  , count :: Int
  } deriving (Show, Generic)

instance FromJSON Pagination
instance ToJSON Pagination

getStart :: Pagination -> Int
getStart pagination = offset pagination + 1

getEnd :: Pagination -> Int
getEnd pagination = offset pagination + count pagination

isFirstPage :: Pagination -> Bool
isFirstPage pagination = offset pagination == 0

isLastPage :: Pagination -> Bool
isLastPage pagination = getEnd pagination == total pagination

previousPageOffset :: Pagination -> Int
previousPageOffset pagination = offset pagination - limit pagination

nextPageOffset :: Pagination -> Int
nextPageOffset pagination = offset pagination + limit pagination
