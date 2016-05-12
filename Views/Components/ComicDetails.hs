{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.ComicDetails (comicDetailsView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Comic
  ( Comic
  , getMarvelUrl
  , getNonEmptyDescription
  , hasCharacters
  , getCharacters
  )
import qualified Models.Comic as C
import Models.CharacterSummary (CharacterSummary, getId)
import qualified Models.CharacterSummary as CS
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
        if hasCharacters comic
          then comicCharactersView comic
          else mempty

comicDescriptionView :: Comic -> Html
comicDescriptionView comic =
  case getNonEmptyDescription comic of
    Nothing -> mempty
    Just description -> do
      H.h4 "Description"
      H.p (toHtml description)

comicCharactersView :: Comic -> Html
comicCharactersView comic = do
  H.h4 "Comics"
  H.ul $
    mapM_ comicCharacterView (getCharacters comic)

comicCharacterView :: CharacterSummary -> Html
comicCharacterView characterSummary =
  H.li $
    case getId characterSummary of
      Just characterId ->
        H.a ! A.href ("/characters/" <> toValue characterId) $
          toHtml (CS.name characterSummary)
      Nothing ->
        toHtml (CS.name characterSummary)
