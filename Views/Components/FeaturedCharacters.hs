{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.FeaturedCharacters (featuredCharactersView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.FeaturedCharacter (FeaturedCharacter)
import qualified Models.FeaturedCharacter as FCh
import Models.Image (getStandardXLarge)

featuredCharactersView :: [FeaturedCharacter] -> Html
featuredCharactersView featuredCharacters =
  case featuredCharacters of
    [] -> mempty
    xs -> do
      H.h2 "Featured characters"
      H.div ! A.class_ "container row" $
        mapM_ featuredCharactersItemView xs

featuredCharactersItemView :: FeaturedCharacter -> Html
featuredCharactersItemView featuredCharacter =
  let name = FCh.name featuredCharacter
  in H.div ! A.class_ "col-md-3" $
    H.a ! A.href ("/characters/" <> toValue (FCh.id featuredCharacter))
      ! A.class_ "thumbnail" $ do
      H.img ! A.class_ "img-responsive img-circle center-block"
            ! A.src (toValue (getStandardXLarge (FCh.thumbnail featuredCharacter)))
            ! A.alt (toValue name)
            ! A.title (toValue name)
      H.div ! A.class_ "caption" $
        H.h4 ! A.class_ "text-center text-ellipsis" $ toHtml name
