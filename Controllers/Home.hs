{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home (getHome) where

import BasicPrelude

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html)

import Config (ConfigM)
import Helpers.PageTitle (makePageTitle)
import Views.Pages.Home (homePageView)

getHome :: ActionT TL.Text ConfigM ()
getHome = do
  let pageTitle = makePageTitle Nothing
  html (renderHtml (homePageView pageTitle))
