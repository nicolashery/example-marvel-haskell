{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.NotFound (notFoundView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Views.Layout (layoutView)

notFoundView :: Text -> Html
notFoundView rootPath =
  let pageTitle = "Not Found"
  in layoutView rootPath pageTitle $ do
    H.div ! A.class_ "page-header" $ H.h1 "Not found"
    H.p $ "We couldn't find what you were looking for."
