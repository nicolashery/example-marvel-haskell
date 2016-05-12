{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.CharacterDetails (characterDetailsView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Character
  ( Character
  , getMarvelUrl
  , getNonEmptyDescription
  , hasComics
  , getComics
  )
import qualified Models.Character as C
import Models.ComicSummary (ComicSummary, getId)
import qualified Models.ComicSummary as CS
import Models.Image (getPortraitXLarge)

characterDetailsView :: Character -> Html
characterDetailsView character =
  H.div ! A.class_ "media" $ do
    H.div ! A.class_ "media-left" $
      H.img ! A.src (toValue (getPortraitXLarge (C.thumbnail character)))
            ! A.alt (toValue (C.name character))
            ! A.title (toValue (C.name character))
    H.div ! A.class_ "media-body" $ do
        H.p $
          H.a ! A.href (toValue (getMarvelUrl character))
              ! A.target "_blank" $
            "More details on Marvel.com"
        characterDescriptionView character
        if hasComics character
          then characterComicsView character
          else mempty

characterDescriptionView :: Character -> Html
characterDescriptionView character =
  case getNonEmptyDescription character of
    Nothing -> mempty
    Just description -> do
      H.h4 "Description"
      H.p (toHtml description)

characterComicsView :: Character -> Html
characterComicsView character = do
  H.h4 "Comics"
  H.ul $
    mapM_ characterComicView (getComics character)

characterComicView :: ComicSummary -> Html
characterComicView comicSummary =
  H.li $
    case getId comicSummary of
      Just comicId ->
        H.a ! A.href ("/comics/" <> toValue comicId) $
          toHtml (CS.name comicSummary)
      Nothing ->
        toHtml (CS.name comicSummary)
