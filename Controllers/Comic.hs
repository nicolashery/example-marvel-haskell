{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Comic
  ( getComics
  , getComic
  ) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (status404, status500)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html, param, rescue, status)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Models.Comic (ComicId)
import qualified Models.Comic as C
import Routes (Route(ComicsRoute, ComicRoute))
import Services.Marvel
  ( findAllComics
  , defaultPaginationOptions
  , PaginationOptions
  , findComic
  )
import qualified Services.Marvel as Mvl
import Views.Pages.Comic (comicPageView)
import Views.Pages.Comics (comicsPageView)
import Views.Pages.Error (errorView)
import Views.Pages.NotFound (notFoundView)

getComics :: ActionT TL.Text ConfigM ()
getComics = do
  _offset :: Int <- param "offset" `rescue` (\_ -> return 0)
  let paginationOptions = getPaginationOptions _offset
  let currentRoute = ComicsRoute
  let pageTitle = makePageTitle (Just "Comics")
  result <- lift (findAllComics paginationOptions)
  case result of
    Left err -> do
      status status500
      html (renderHtml (errorView currentRoute (show err)))
    Right response ->
      html (renderHtml (comicsPageView
        currentRoute pageTitle (Mvl.comicsPagination response) (Mvl.comics response)
      ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }

getComic :: ActionT TL.Text ConfigM ()
getComic = do
  _id :: ComicId <- param "id"
  let currentRoute = ComicRoute
  result <- lift (findComic _id)
  case result of
    Left Mvl.NotFound -> do
      status status404
      html (renderHtml (notFoundView currentRoute))
    Left err -> do
      status status500
      html (renderHtml (errorView currentRoute (show err)))
    Right response ->
      let comicName = C.title (Mvl.comic response)
          pageTitle = makePageTitle (Just comicName)
      in html (renderHtml (comicPageView
        currentRoute pageTitle (Mvl.comic response)
      ))
