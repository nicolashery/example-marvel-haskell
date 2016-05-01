{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Characters (getCharacters) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, text, html, param, rescue)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Services.Marvel
  ( findAllCharacters
  , defaultPaginationOptions
  , PaginationOptions
  )
import qualified Services.Marvel as Mvl
import Views.Pages.Characters (charactersPageView)

getCharacters :: ActionT TL.Text ConfigM ()
getCharacters = do
  _offset :: Int <- param "offset" `rescue` (\_ -> return 0)
  let paginationOptions = getPaginationOptions _offset
  let pageTitle = makePageTitle (Just "Characters")
  result <- lift (findAllCharacters paginationOptions)
  case result of
    Left err ->
      text (TL.pack err)
    Right response ->
      html (renderHtml (charactersPageView
        pageTitle (Mvl.pagination response) (Mvl.characters response)
      ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }
