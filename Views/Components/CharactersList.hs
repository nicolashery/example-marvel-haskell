{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.CharactersList (charactersListView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toValue, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Character (Character)
import qualified Models.Character as C

charactersListView :: [Character] -> Html
charactersListView characters =
  H.div ! A.class_ "list-group" $
    mapM_ charactersListItemView characters

charactersListItemView :: Character -> Html
charactersListItemView character =
  H.a ! A.href ("/characters/" <> toValue (C.id character)) ! A.class_ "list-group-item" $
    H.h4 ! A.class_ "list-group-item-heading" $ toHtml (C.name character)
