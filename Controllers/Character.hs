{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Character
  ( getCharacters
  , getCharacter
  ) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html, param, request, rescue)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Helpers.PathInfo (getRootPath)
import qualified Models.Character as C
import Services.Marvel
  ( findAllCharacters
  , defaultPaginationOptions
  , PaginationOptions
  , findCharacter
  )
import qualified Services.Marvel as Mvl
import Views.Pages.Character (characterPageView)
import Views.Pages.Characters (charactersPageView)
import Views.Pages.Error (errorView)
import Views.Pages.NotFound (notFoundView)

getCharacters :: ActionT TL.Text ConfigM ()
getCharacters = do
  _offset :: Int <- param "offset" `rescue` (\_ -> return 0)
  let paginationOptions = getPaginationOptions _offset
  req <- request
  let rootPath = getRootPath req
  let pageTitle = makePageTitle (Just "Characters")
  result <- lift (findAllCharacters paginationOptions)
  case result of
    Left err ->
      html (renderHtml (errorView rootPath (show err)))
    Right response ->
      html (renderHtml (charactersPageView
        rootPath pageTitle (Mvl.charactersPagination response) (Mvl.characters response)
      ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }

getCharacter :: ActionT TL.Text ConfigM ()
getCharacter = do
  _id :: Int <- param "id"
  req <- request
  let rootPath = getRootPath req
  result <- lift (findCharacter _id)
  case result of
    Left Mvl.NotFound ->
      html (renderHtml (notFoundView rootPath))
    Left err ->
      html (renderHtml (errorView rootPath (show err)))
    Right response ->
      let characterName = C.name (Mvl.character response)
          pageTitle = makePageTitle (Just characterName)
      in html (renderHtml (characterPageView
        rootPath pageTitle (Mvl.character response)
      ))
