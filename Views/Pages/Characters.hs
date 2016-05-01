{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Characters
  ( charactersPageView
  , charactersPageContentView
  ) where

import BasicPrelude

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Character (Character)
import Models.Pagination (Pagination)
import Views.Components.CharactersList (charactersListView)
import Views.Components.ResultsPagination (resultsPaginationView)
import Views.Layout (layoutView)

charactersPageView :: Text -> Pagination -> [Character] -> Html
charactersPageView pageTitle pagination characters =
  layoutView pageTitle (charactersPageContentView pagination characters)

charactersPageContentView :: Pagination -> [Character] -> Html
charactersPageContentView pagination characters = do
  H.div ! A.class_ "page-header" $ H.h1 "Characters"
  resultsPaginationView pagination
  charactersListView characters
