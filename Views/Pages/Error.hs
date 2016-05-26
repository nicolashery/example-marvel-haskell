{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Error (errorView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Views.Layout (layoutView)

errorView :: Text -> Text -> Html
errorView rootPath err =
  let pageTitle = "Error"
  in layoutView rootPath pageTitle $ do
    H.div ! A.class_ "page-header" $ H.h1 "Error"
    H.p $ "An unexpected error occured."
    H.p $ toHtml err
