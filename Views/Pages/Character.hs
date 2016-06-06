{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Character
  ( characterPageView
  , characterPageContentView
  ) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Helpers.SPF (SPFHook(SPFLink))
import Models.Character (Character)
import qualified Models.Character as C
import Routes
  ( Route
  , RouteUrl(CharactersUrl)
  , emptyPaginationQuery
  )
import Views.Components.CharacterDetails (characterDetailsView)
import Views.Layout (layoutView)

characterPageView :: Route -> Text -> Character -> Html
characterPageView currentRoute pageTitle character =
  layoutView currentRoute pageTitle (characterPageContentView character)

characterPageContentView :: Character -> Html
characterPageContentView character = do
  let charactersUrl = toValue (CharactersUrl emptyPaginationQuery)
  H.a ! A.class_ (toValue SPFLink) ! A.href charactersUrl $ "Browse all characters"
  H.div ! A.class_ "page-header" $ H.h1 (toHtml (C.name character))
  characterDetailsView character
