{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home (getHome) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Routes (Route(HomeRoute))
import Services.Marvel (fetchFeaturedCharacters, fetchFeaturedComics)
import qualified Services.Marvel as Mvl
import Views.Pages.Home (homePageView)

getHome :: ActionT TL.Text ConfigM ()
getHome = do
  let currentRoute = HomeRoute
  let pageTitle = makePageTitle Nothing
  featuredCharactersResult <- lift fetchFeaturedCharacters
  let featuredCharacters =
        -- Default to empty list if error
        either (\_ -> []) Mvl.featuredCharacters featuredCharactersResult
  featuredComicsResult <- lift fetchFeaturedComics
  let featuredComics =
        either (\_ -> []) Mvl.featuredComics featuredComicsResult
  html (renderHtml (homePageView
      currentRoute pageTitle featuredCharacters featuredComics
    ))
