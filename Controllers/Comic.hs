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
import Web.Scotty.Trans (ActionT, html, json, param, rescue, status)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Helpers.SPF
  ( SPFHook(SPFNavbarComics)
  , SPFQuery(SPFNavigate, SPFNoop)
  )
import qualified Helpers.SPF as SPF
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
import Views.Pages.Comic
  ( comicPageView
  , comicPageContentView
  )
import Views.Pages.Comics
  ( comicsPageView
  , comicsPageContentView
  )
import Views.Pages.Error (errorView)
import Views.Pages.NotFound (notFoundView)

getComics :: ActionT TL.Text ConfigM ()
getComics = do
  spf :: SPFQuery <- param "spf" `rescue` (\_ -> return SPFNoop)
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
      if spf == SPFNavigate
      then json SPF.SPFResponse
        { SPF.pageTitle=pageTitle
        , SPF.activeNavbarItem=Just SPFNavbarComics
        , SPF.contentHtml=renderHtml (comicsPageContentView 
            (Mvl.comicsPagination response) (Mvl.comics response)
          )
        }
      else html (renderHtml (comicsPageView
          currentRoute pageTitle (Mvl.comicsPagination response) (Mvl.comics response)
        ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }

getComic :: ActionT TL.Text ConfigM ()
getComic = do
  spf :: SPFQuery <- param "spf" `rescue` (\_ -> return SPFNoop)
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
      in if spf == SPFNavigate
        then json SPF.SPFResponse
          { SPF.pageTitle=pageTitle
          , SPF.activeNavbarItem=Just SPFNavbarComics
          , SPF.contentHtml=renderHtml (comicPageContentView
              (Mvl.comic response)
            )
          }
        else  html (renderHtml (comicPageView
            currentRoute pageTitle (Mvl.comic response)
          ))
