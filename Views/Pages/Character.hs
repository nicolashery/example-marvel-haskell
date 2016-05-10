{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Character
  ( characterPageView
  , characterPageContentView
  ) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Character (Character)
import qualified Models.Character as C
import Views.Components.CharacterDetails (characterDetailsView)
import Views.Layout (layoutView)

characterPageView :: Text -> Text -> Character -> Html
characterPageView rootPath pageTitle character =
  layoutView rootPath pageTitle (characterPageContentView character)

characterPageContentView :: Character -> Html
characterPageContentView character = do
  H.a ! A.href "/characters" $ "Browse all characters"
  H.div ! A.class_ "page-header" $ H.h1 (toHtml (C.name character))
  characterDetailsView character
