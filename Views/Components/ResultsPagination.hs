{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.ResultsPagination (resultsPaginationView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.Pagination (Pagination)
import qualified Models.Pagination as P

resultsPaginationView :: Pagination -> Html
resultsPaginationView pagination = do
  let start = show (P.getStart pagination)
  let end = show (P.getEnd pagination)
  let total = show (P.total pagination)
  let previousOffset = show (P.previousPageOffset pagination)
  let nextOffset = show (P.nextPageOffset pagination)
  H.nav $ H.ul ! A.class_ "pager" $ do
    H.li $ do
        H.em "Showing "
        H.strong (toHtml (start ++ " - " ++ end))
        H.em " of "
        H.strong (toHtml total)
    H.li ! A.class_ (if P.isFirstPage pagination then "disabled" else "") $
      H.a ! A.href (toValue ("/characters?offset=" ++ previousOffset)) $
        "Previous"
    H.li ! A.class_ (if P.isLastPage pagination then "disabled" else "") $
      H.a ! A.href (toValue ("/characters?offset=" ++ nextOffset)) $
        "Next"
