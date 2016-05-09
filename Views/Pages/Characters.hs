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

charactersPageView :: Text -> Text -> Pagination -> [Character] -> Html
charactersPageView rootPath pageTitle pagination characters =
  layoutView rootPath pageTitle (charactersPageContentView rootPath pagination characters)

charactersPageContentView :: Text -> Pagination -> [Character] -> Html
charactersPageContentView rootPath pagination characters = do
  H.div ! A.class_ "page-header" $ H.h1 "Characters"
  resultsPaginationView rootPath pagination
  charactersListView characters
