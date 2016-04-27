{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.MainFooter (mainFooterView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

mainFooterView :: Html
mainFooterView =
  H.footer $ H.div ! A.class_ "container" $ H.div ! A.class_ "row" $ do
    H.div ! A.class_ "col-md-6" $
      H.a
        ! A.href "http://marvel.com"
        ! A.target "_blank"
        $ "Data provided by Marvel. © 2016 MARVEL"
    H.div ! A.class_ "col-md-6" $
      H.a
        ! A.href "https://github.com/nicolashery/example-marvel-app"
        ! A.target "_blank"
        $ "View on GitHub"
