{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Layout (layoutView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Routes (Route)
import Views.Components.MainFooter (mainFooterView)
import Views.Components.MainNavigation (mainNavigationView)

layoutView :: Route -> Text -> Html -> Html
layoutView currentRoute pageTitle bodyContent = H.docTypeHtml $ do
  H.head $ do
    H.title (toHtml pageTitle)
    H.meta ! A.charset "utf-8"
    H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
    H.meta ! A.name "description" ! A.content ""
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
    H.link ! A.rel "stylesheet" ! A.href "/css/app.css"
  H.body $ do
    mainNavigationView currentRoute
    H.div ! A.class_ "container" $ bodyContent
    mainFooterView
