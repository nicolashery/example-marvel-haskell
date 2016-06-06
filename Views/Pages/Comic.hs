{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Comic
  ( comicPageView
  , comicPageContentView
  ) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Helpers.SPF (SPFHook(SPFLink))
import Models.Comic (Comic)
import qualified Models.Comic as C
import Routes
  ( Route
  , RouteUrl(ComicsUrl)
  , emptyPaginationQuery
  )
import Views.Components.ComicDetails (comicDetailsView)
import Views.Layout (layoutView)

comicPageView :: Route -> Text -> Comic -> Html
comicPageView currentRoute pageTitle comic =
  layoutView currentRoute pageTitle (comicPageContentView comic)

comicPageContentView :: Comic -> Html
comicPageContentView comic = do
  let comicsUrl = toValue (ComicsUrl emptyPaginationQuery)
  H.a ! A.class_ (toValue SPFLink) ! A.href comicsUrl $ "Browse all comics"
  H.div ! A.class_ "page-header" $ H.h1 (toHtml (C.title comic))
  comicDetailsView comic
