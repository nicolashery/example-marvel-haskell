{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Characters
  ( charactersPageView
  , charactersPageContentView
  ) where

import BasicPrelude

import Text.Blaze (AttributeValue)
import Text.Blaze.Html5 (Html, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Character (Character)
import Models.Pagination (Pagination)
import Routes 
  ( Route
  , RouteUrl(CharactersUrl)
  , PaginationQuery(..)
  )
import Views.Components.CharactersList (charactersListView)
import Views.Components.ResultsPagination (resultsPaginationView)
import Views.Layout (layoutView)

charactersPageView :: Route -> Text -> Pagination -> [Character] -> Html
charactersPageView currentRoute pageTitle pagination characters =
  layoutView currentRoute pageTitle
    (charactersPageContentView pagination characters)

charactersPageContentView :: Pagination -> [Character] -> Html
charactersPageContentView pagination characters = do
  H.div ! A.class_ "page-header" $ H.h1 "Characters"
  resultsPaginationView makePaginationUrl pagination
  charactersListView characters

makePaginationUrl :: Int -> AttributeValue
makePaginationUrl _offset =
  toValue (CharactersUrl PaginationQuery { offset=Just _offset })
