{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Pages.Home (homePageView) where

import BasicPrelude

import Text.Blaze.Html5 (Html, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Models.FeaturedCharacter (FeaturedCharacter)
import Models.FeaturedComic (FeaturedComic)
import Routes
  ( Route
  , RouteUrl(CharactersUrl, ComicsUrl)
  , emptyPaginationQuery
  )
import Views.Components.FeaturedCharacters (featuredCharactersView)
import Views.Components.FeaturedComics (featuredComicsView)
import Views.Layout (layoutView)

homePageView :: Route -> Text -> [FeaturedCharacter] -> [FeaturedComic] -> Html
homePageView currentRoute pageTitle featuredCharacters featuredComics =
  layoutView currentRoute pageTitle $ do
    H.div ! A.class_ "jumbotron" $ do
      H.h1 "Marvel App"
      H.p $ do
          H.span "This is an example app using the "
          H.a
            ! A.href "https://developer.marvel.com/"
            ! A.target "_blank"
            $ "Marvel API"
          H.span " data."
      H.p $ do
          H.a
            ! A.class_ "btn btn-primary btn-lg"
            ! A.href (toValue (CharactersUrl emptyPaginationQuery))
            $ "Browse characters"
          H.span " "
          H.a
            ! A.class_ "btn btn-primary btn-lg"
            ! A.href (toValue (ComicsUrl emptyPaginationQuery))
            $ "Browse comics"
    featuredCharactersView featuredCharacters
    featuredComicsView featuredComics
