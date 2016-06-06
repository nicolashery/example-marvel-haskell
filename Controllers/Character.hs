{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Character
  ( getCharacters
  , getCharacter
  ) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (status404, status500)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html, json, param, rescue, status)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Helpers.SPF
  ( SPFHook(SPFNavbarCharacters)
  , SPFQuery(SPFNavigate, SPFNoop)
  )
import qualified Helpers.SPF as SPF
import Models.Character (CharacterId)
import qualified Models.Character as C
import Routes (Route(CharactersRoute, CharacterRoute))
import Services.Marvel
  ( findAllCharacters
  , defaultPaginationOptions
  , PaginationOptions
  , findCharacter
  )
import qualified Services.Marvel as Mvl
import Views.Pages.Character
  ( characterPageView
  , characterPageContentView
  )
import Views.Pages.Characters
  ( charactersPageView
  , charactersPageContentView
  )
import Views.Pages.Error (errorView)
import Views.Pages.NotFound (notFoundView)

getCharacters :: ActionT TL.Text ConfigM ()
getCharacters = do
  spf :: SPFQuery <- param "spf" `rescue` (\_ -> return SPFNoop)
  _offset :: Int <- param "offset" `rescue` (\_ -> return 0)
  let paginationOptions = getPaginationOptions _offset
  let currentRoute = CharactersRoute
  let pageTitle = makePageTitle (Just "Characters")
  result <- lift (findAllCharacters paginationOptions)
  case result of
    Left err -> do
      status status500
      html (renderHtml (errorView currentRoute (show err)))
    Right response ->
      if spf == SPFNavigate
      then json SPF.SPFResponse
        { SPF.pageTitle=pageTitle
        , SPF.activeNavbarItem=Just SPFNavbarCharacters
        , SPF.contentHtml=renderHtml (charactersPageContentView 
            (Mvl.charactersPagination response) (Mvl.characters response)
          )
        }
      else html (renderHtml (charactersPageView
          currentRoute pageTitle (Mvl.charactersPagination response) (Mvl.characters response)
        ))

getPaginationOptions :: Int -> PaginationOptions
getPaginationOptions _offset = Mvl.PaginationOptions
  { Mvl.limit=Mvl.limit defaultPaginationOptions
  , Mvl.offset=_offset
  }

getCharacter :: ActionT TL.Text ConfigM ()
getCharacter = do
  spf :: SPFQuery <- param "spf" `rescue` (\_ -> return SPFNoop)
  _id :: CharacterId <- param "id"
  let currentRoute = CharacterRoute
  result <- lift (findCharacter _id)
  case result of
    Left Mvl.NotFound -> do
      status status404
      html (renderHtml (notFoundView currentRoute))
    Left err -> do
      status status500
      html (renderHtml (errorView currentRoute (show err)))
    Right response ->
      let characterName = C.name (Mvl.character response)
          pageTitle = makePageTitle (Just characterName)
       in if spf == SPFNavigate
        then json SPF.SPFResponse
          { SPF.pageTitle=pageTitle
          , SPF.activeNavbarItem=Just SPFNavbarCharacters
          , SPF.contentHtml=renderHtml (characterPageContentView
              (Mvl.character response)
            )
          }
        else html (renderHtml (characterPageView
            currentRoute pageTitle (Mvl.character response)
          ))
