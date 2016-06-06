{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Layout (layoutView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Helpers.SPF (SPFHook(SPFContent))
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
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/nprogress/0.2.0/nprogress.min.css"
    H.link ! A.rel "stylesheet" ! A.href "/css/app.css"
  H.body $ do
    mainNavigationView currentRoute
    H.div ! A.id (toValue SPFContent) ! A.class_ "container" $ bodyContent
    mainFooterView
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/spf/2.3.2/spf.js" $ mempty
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/nprogress/0.2.0/nprogress.min.js" $ mempty
    H.script ! A.src "/js/app.js" $ mempty
