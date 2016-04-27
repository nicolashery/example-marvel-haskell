{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.MainNavigation (mainNavigationView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

mainNavigationView :: Html
mainNavigationView =
  H.nav ! A.class_ "navbar navbar-default" $ H.div ! A.class_ "container" $ do
    H.div ! A.class_ "navbar-header" $
      H.a ! A.href "/" ! A.class_ "navbar-brand" $ "Marvel App"
    H.ul ! A.class_ "nav navbar-nav" $ do
      H.li $ H.a ! A.href "/characters" $ "Characters"
      H.li $ H.a ! A.href "/comics" $ "Comics"
