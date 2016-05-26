{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Home (homePageView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.FeaturedCharacter (FeaturedCharacter)
import Models.FeaturedComic (FeaturedComic)
import Views.Components.FeaturedCharacters (featuredCharactersView)
import Views.Components.FeaturedComics (featuredComicsView)
import Views.Layout (layoutView)

homePageView :: Text -> Text -> [FeaturedCharacter] -> [FeaturedComic] -> Html
homePageView rootPath pageTitle featuredCharacters featuredComics =
  layoutView rootPath pageTitle $ do
    H.div ! A.class_ "jumbotron" $ do
      H.h1 "Marvel App"
      H.p $ do
          H.span "This is an example app using the "
          H.a ! A.href "https://developer.marvel.com/" ! A.target "_blank" $ "Marvel API"
          H.span " data."
      H.p $ do
          H.a ! A.class_ "btn btn-primary btn-lg" ! A.href "/characters" $ "Browse characters"
          H.span " "
          H.a ! A.class_ "btn btn-primary btn-lg" ! A.href "/comics" $ "Browse comics"
    featuredCharactersView featuredCharacters
    featuredComicsView featuredComics
