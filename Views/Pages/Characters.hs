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
import Views.Layout (layoutView)
import Views.Components.CharactersList (charactersListView)

charactersPageView :: Text -> [Character] -> Html
charactersPageView pageTitle characters =
  layoutView pageTitle (charactersPageContentView characters)

charactersPageContentView :: [Character] -> Html
charactersPageContentView characters = do
  H.div ! A.class_ "page-header" $ H.h1 "Characters"
  charactersListView characters
