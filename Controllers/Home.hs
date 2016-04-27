{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home (getHome) where

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (ActionT, html)

import Config (ConfigM)
import Views.Pages.Home (homePageView)

getHome :: ActionT TL.Text ConfigM ()
getHome =
  html (renderHtml (homePageView "Marvel App"))
