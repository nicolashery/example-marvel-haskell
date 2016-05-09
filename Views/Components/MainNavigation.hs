{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.MainNavigation (mainNavigationView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

mainNavigationView :: Text -> Html
mainNavigationView rootPath =
  H.nav ! A.class_ "navbar navbar-default" $ H.div ! A.class_ "container" $ do
    H.div ! A.class_ "navbar-header" $
      H.a ! A.href "/" ! A.class_ "navbar-brand" $ "Marvel App"
    H.ul ! A.class_ "nav navbar-nav" $ do
      navItemView rootPath "/characters" "Characters"
      navItemView rootPath "/comics" "Comics"

navItemView :: Text -> Text -> Html -> Html
navItemView rootPath itemPath label =
  let classes = if rootPath == itemPath then "active" else ""
  in H.li ! A.class_ classes $ H.a ! A.href (toValue itemPath) $ label
