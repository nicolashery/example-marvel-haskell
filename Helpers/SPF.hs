{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers.SPF
  ( SPFHook(..)
  , SPFQuery(..)
  , SPFResponse(..)
  ) where

import BasicPrelude hiding (show)
import BasicPrelude as BP
import Prelude (show)

import Data.Aeson (ToJSON(..), object)
import qualified Data.Aeson as JSON
import Data.Aeson.Types ((.=))
import qualified Data.Text.Lazy as TL
import Text.Blaze (ToValue, toValue)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, string)
import Web.Scotty (Parsable(..), readEither)

tshow :: (Show a) => a -> Text
tshow = BP.show

data SPFHook =
    SPFLink
  | SPFContent
  | SPFNavbarCharacters
  | SPFNavbarComics
  deriving (Eq)

instance Show SPFHook where
  show SPFLink = "spf-link"
  show SPFContent = "spf-content"
  show SPFNavbarCharacters = "spf-navbar-characters"
  show SPFNavbarComics = "spf-navbar-comics"

instance ToValue SPFHook where
  toValue spfHook = toValue (tshow spfHook)

data SPFQuery =
    SPFNavigate
  | SPFLoad
  | SPFNoop
  deriving (Show, Eq)

instance Read SPFQuery where
  readsPrec _ = readP_to_S parseSpfQuery
    where
      parseSpfQuery :: ReadP SPFQuery
      parseSpfQuery = do
        str <- string "navigate" <|> string "load"
        case str of
          "navigate" -> return SPFNavigate
          "load" -> return SPFLoad
          -- This should never be reached, but including
          -- to supress non-exhaustive pattern-match warning
          _ -> return SPFNoop

instance Parsable SPFQuery where
  parseParam = readEither

data SPFResponse = SPFResponse
  { pageTitle :: Text
  , activeNavbarItem :: Maybe SPFHook
  , contentHtml :: TL.Text
  } deriving (Show)

instance ToJSON SPFResponse where
  toJSON r =
    let title = pageTitle r
        body = object [(tshow SPFContent) .= contentHtml r]
        _activeNavbarItem = activeNavbarItem r
        attr = object
          [ (tshow SPFNavbarCharacters) .=
            object ["class" .= navbarHookClass SPFNavbarCharacters _activeNavbarItem]
          , (tshow SPFNavbarComics) .=
            object ["class" .= navbarHookClass SPFNavbarComics _activeNavbarItem]
          ]
    in object
      [ "title" .= title
      , "attr" .= attr
      , "body" .= body
      ]

navbarHookClass :: SPFHook -> Maybe SPFHook -> Text
navbarHookClass _ Nothing = ""
navbarHookClass spfNavbarHook (Just _activeNavbarItem) =
  if spfNavbarHook == _activeNavbarItem then "active" else ""

instance ToJSON SPFHook where
  toJSON spfHook = JSON.String (tshow spfHook)
