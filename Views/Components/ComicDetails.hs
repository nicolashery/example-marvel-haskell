{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.ComicDetails (comicDetailsView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Comic (Comic, getMarvelUrl, getNonEmptyDescription)
import qualified Models.Comic as C
import Models.Image (getPortraitXLarge)

comicDetailsView :: Comic -> Html
comicDetailsView comic =
  H.div ! A.class_ "media" $ do
    H.div ! A.class_ "media-left" $
      H.img ! A.src (toValue (getPortraitXLarge (C.thumbnail comic)))
            ! A.alt (toValue (C.title comic))
            ! A.title (toValue (C.title comic))
    H.div ! A.class_ "media-body" $ do
        H.p $
          H.a ! A.href (toValue (getMarvelUrl comic))
              ! A.target "_blank" $
            "More details on Marvel.com"
        comicDescriptionView comic

comicDescriptionView :: Comic -> Html
comicDescriptionView comic =
  case getNonEmptyDescription comic of
    Nothing -> mempty
    Just description -> do
      H.h4 "Description"
      H.p (toHtml description)
