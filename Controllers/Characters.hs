{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Characters (getCharacters) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, text, html)

import Config (ConfigM)
import Services.Marvel (findAllCharacters, defaultPaginationOptions)
import qualified Services.Marvel as Mvl
import Views.Pages.Characters (charactersPageView)

getCharacters :: ActionT TL.Text ConfigM ()
getCharacters = do
  result <- lift (findAllCharacters defaultPaginationOptions)
  case result of
    Left err ->
      text (TL.pack err)
    Right response ->
      html (renderHtml (charactersPageView "Characters" (Mvl.characters response)))
