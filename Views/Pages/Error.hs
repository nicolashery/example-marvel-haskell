{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Error (errorView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Routes (Route)
import Views.Layout (layoutView)

errorView :: Route -> Text -> Html
errorView currentRoute err =
  let pageTitle = "Error"
  in layoutView currentRoute pageTitle $ do
    H.div ! A.class_ "page-header" $ H.h1 "Error"
    H.p $ "An unexpected error occured."
    H.p $ toHtml err
