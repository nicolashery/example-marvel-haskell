{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.FeaturedComics (featuredComicsView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.FeaturedComic (FeaturedComic)
import qualified Models.FeaturedComic as FCo
import Models.Image (getStandardXLarge)

featuredComicsView :: [FeaturedComic] -> Html
featuredComicsView featuredComics =
  case featuredComics of
    [] -> mempty
    xs -> do
      H.h2 "Featured comics"
      H.div ! A.class_ "container row" $
        mapM_ featuredComicsItemView xs

featuredComicsItemView :: FeaturedComic -> Html
featuredComicsItemView featuredComic =
  let name = FCo.name featuredComic
  in H.div ! A.class_ "col-md-3" $
    H.a ! A.href ("/comics/" <> toValue (FCo.id featuredComic))
      ! A.class_ "thumbnail" $ do
      H.img ! A.class_ "img-responsive img-circle center-block"
            ! A.src (toValue (getStandardXLarge (FCo.thumbnail featuredComic)))
            ! A.alt (toValue name)
            ! A.title (toValue name)
      H.div ! A.class_ "caption" $
        H.h4 ! A.class_ "text-center text-ellipsis" $ toHtml name
