{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.ComicsList (comicsListView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toValue, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Comic (Comic)
import qualified Models.Comic as C
import Routes (RouteUrl(ComicUrl))

comicsListView :: [Comic] -> Html
comicsListView comics =
  H.div ! A.class_ "list-group" $
    mapM_ comicsListItemView comics

comicsListItemView :: Comic -> Html
comicsListItemView comic =
  H.a ! A.href (toValue (ComicUrl (C.id comic))) ! A.class_ "list-group-item" $
    H.h4 ! A.class_ "list-group-item-heading" $ toHtml (C.title comic)
