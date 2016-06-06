{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Components.MainNavigation (mainNavigationView) where

import BasicPrelude

import Text.Blaze (AttributeValue)
import Text.Blaze.Html5 (Html, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Helpers.SPF
  ( SPFHook(SPFLink, SPFNavbarCharacters, SPFNavbarComics)
  )
import Routes
  ( Route(CharactersRoute, CharacterRoute, ComicsRoute, ComicRoute)
  , RouteUrl(HomeUrl, CharactersUrl, ComicsUrl)
  , emptyPaginationQuery
  )

mainNavigationView :: Route -> Html
mainNavigationView currentRoute =
  H.nav ! A.class_ "navbar navbar-default" $ H.div ! A.class_ "container" $ do
    H.div ! A.class_ "navbar-header" $
      H.a ! A.href (toValue HomeUrl) ! A.class_ "navbar-brand" $ "Marvel App"
    H.ul ! A.class_ "nav navbar-nav" $ do
      navItemView
        SPFNavbarCharacters
        (currentRoute == CharactersRoute || currentRoute == CharacterRoute)
        (toValue (CharactersUrl emptyPaginationQuery))
        "Characters"
      navItemView
        SPFNavbarComics
        (currentRoute == ComicsRoute || currentRoute == ComicRoute)
        (toValue (ComicsUrl emptyPaginationQuery))
        "Comics"

navItemView :: SPFHook -> Bool -> AttributeValue -> Html -> Html
navItemView spfNavbarHook isActive itemUrl label =
  let activeClass = if isActive then "active" else ""
  in
    H.li ! A.id (toValue spfNavbarHook) ! A.class_ activeClass $
      H.a ! A.class_ (toValue SPFLink) ! A.href itemUrl $ label
