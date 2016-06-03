{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Routes
  ( Route(..)
  , RouteUrl(..)
  , PaginationQuery(..)
  , emptyPaginationQuery
  , toDefaultUrl
  , toRoute
  , toPattern
  ) where

import BasicPrelude

import Text.Blaze (ToValue, toValue)
import Web.Scotty.Internal.Types (RoutePattern)

import Models.Ids (CharacterId, ComicId)

data Route =
    HomeRoute
  | CharactersRoute
  | CharacterRoute
  | ComicsRoute
  | ComicRoute
  deriving (Show, Eq)

data RouteUrl =
    HomeUrl
  | CharactersUrl PaginationQuery
  | CharacterUrl CharacterId
  | ComicsUrl PaginationQuery
  | ComicUrl ComicId
  deriving (Show)

data PaginationQuery = PaginationQuery
  { offset :: Maybe Int
  } deriving (Show)
  
emptyPaginationQuery :: PaginationQuery
emptyPaginationQuery = PaginationQuery
  { offset=Nothing }
  
-- Makes sure each Route has a corresponding URL type
toDefaultUrl :: Route -> RouteUrl
toDefaultUrl HomeRoute = HomeUrl
toDefaultUrl CharactersRoute = CharactersUrl PaginationQuery { offset=Nothing }
toDefaultUrl CharacterRoute = CharacterUrl 0
toDefaultUrl ComicsRoute = ComicsUrl PaginationQuery { offset=Nothing }
toDefaultUrl ComicRoute = ComicUrl 0

-- Also makes sure each URL type corresponds to a Route
toRoute :: RouteUrl -> Route
toRoute HomeUrl = HomeRoute
toRoute (CharactersUrl _) = CharactersRoute
toRoute (CharacterUrl _) = CharacterRoute
toRoute (ComicsUrl _) = ComicsRoute
toRoute (ComicUrl _) = ComicRoute

-- Use when declaring Scotty routing
toPattern :: Route -> RoutePattern
toPattern HomeRoute = "/"
toPattern CharactersRoute = "/characters"
toPattern CharacterRoute = "/characters/:id"
toPattern ComicsRoute = "/comics"
toPattern ComicRoute = "/comics/:id"

-- Use when building URLs in Blaze views
instance ToValue RouteUrl where

  toValue HomeUrl =
    "/"
  toValue (CharactersUrl PaginationQuery { offset=(Just _offset)}) =
    "/characters?offset=" <> toValue _offset
  toValue (CharactersUrl _) =
    "/characters"
  toValue (CharacterUrl _characterId) =
    "/characters/" <> toValue _characterId
  toValue (ComicsUrl PaginationQuery { offset=(Just _offset)}) =
    "/comics?offset=" <> toValue _offset
  toValue (ComicsUrl _) =
    "/comics"
  toValue (ComicUrl _comicId) =
    "/comics/" <> toValue _comicId
