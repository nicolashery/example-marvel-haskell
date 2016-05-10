{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Comics
  ( comicsPageView
  , comicsPageContentView
  ) where

import BasicPrelude

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Comic (Comic)
import Models.Pagination (Pagination)
import Views.Components.ComicsList (comicsListView)
import Views.Components.ResultsPagination (resultsPaginationView)
import Views.Layout (layoutView)

comicsPageView :: Text -> Text -> Pagination -> [Comic] -> Html
comicsPageView rootPath pageTitle pagination comics =
  layoutView rootPath pageTitle (comicsPageContentView rootPath pagination comics)

comicsPageContentView :: Text -> Pagination -> [Comic] -> Html
comicsPageContentView rootPath pagination comics = do
  H.div ! A.class_ "page-header" $ H.h1 "Comics"
  resultsPaginationView rootPath pagination
  comicsListView comics
