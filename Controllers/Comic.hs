{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Comic
  ( getComics
  , getComic
  ) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html, param, request, rescue)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Helpers.PathInfo (getRootPath)
import qualified Models.Comic as C
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

getComics :: ActionT TL.Text ConfigM ()
getComics = do
  _offset :: Int <- param "offset" `rescue` (\_ -> return 0)
  let paginationOptions = getPaginationOptions _offset
  req <- request
  let rootPath = getRootPath req
  let pageTitle = makePageTitle (Just "Comics")
  result <- lift (findAllComics paginationOptions)
  case result of
    Left err ->
      html (renderHtml (errorView rootPath (show err)))
    Right response ->
      html (renderHtml (comicsPageView
        rootPath pageTitle (Mvl.comicsPagination response) (Mvl.comics response)
      ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }

getComic :: ActionT TL.Text ConfigM ()
getComic = do
  _id :: Int <- param "id"
  req <- request
  let rootPath = getRootPath req
  result <- lift (findComic _id)
  case result of
    Left err ->
      html (renderHtml (errorView rootPath (show err)))
    Right response ->
      let comicName = C.title (Mvl.comic response)
          pageTitle = makePageTitle (Just comicName)
      in html (renderHtml (comicPageView
        rootPath pageTitle (Mvl.comic response)
      ))
