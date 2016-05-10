{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Comics (getComics) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, text, html, param, request, rescue)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Helpers.PathInfo (getRootPath)
import Services.Marvel
  ( findAllComics
  , defaultPaginationOptions
  , PaginationOptions
  )
import qualified Services.Marvel as Mvl
import Views.Pages.Comics (comicsPageView)

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
      text (TL.pack err)
    Right response ->
      html (renderHtml (comicsPageView
        rootPath pageTitle (Mvl.comicsPagination response) (Mvl.comics response)
      ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }
