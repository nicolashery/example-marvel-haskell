{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Comic
  ( comicPageView
  , comicPageContentView
  ) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Comic (Comic)
import qualified Models.Comic as C
import Views.Components.ComicDetails (comicDetailsView)
import Views.Layout (layoutView)

comicPageView :: Text -> Text -> Comic -> Html
comicPageView rootPath pageTitle comic =
  layoutView rootPath pageTitle (comicPageContentView comic)

comicPageContentView :: Comic -> Html
comicPageContentView comic = do
  H.a ! A.href "/comics" $ "Browse all comics"
  H.div ! A.class_ "page-header" $ H.h1 (toHtml (C.title comic))
  comicDetailsView comic
