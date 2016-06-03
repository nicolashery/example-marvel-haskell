{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Comics
  ( comicsPageView
  , comicsPageContentView
  ) where

import BasicPrelude

import Text.Blaze (AttributeValue)
import Text.Blaze.Html5 (Html, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Comic (Comic)
import Models.Pagination (Pagination)
import Routes
  ( Route
  , RouteUrl(ComicsUrl)
  , PaginationQuery(..)
  )
import Views.Components.ComicsList (comicsListView)
import Views.Components.ResultsPagination (resultsPaginationView)
import Views.Layout (layoutView)

comicsPageView :: Route -> Text -> Pagination -> [Comic] -> Html
comicsPageView currentRoute pageTitle pagination comics =
  layoutView currentRoute pageTitle
    (comicsPageContentView pagination comics)

comicsPageContentView :: Pagination -> [Comic] -> Html
comicsPageContentView pagination comics = do
  H.div ! A.class_ "page-header" $ H.h1 "Comics"
  resultsPaginationView makePaginationUrl pagination
  comicsListView comics

makePaginationUrl :: Int -> AttributeValue
makePaginationUrl _offset =
  toValue (ComicsUrl PaginationQuery { offset=Just _offset })
