{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers.PageTitle (makePageTitle) where

import BasicPrelude

siteName :: Text
siteName = "Marvel App"

makePageTitle :: Maybe Text -> Text
makePageTitle maybePageName =
  case maybePageName of
    Nothing -> siteName
    Just pageName -> pageName ++ " | " ++ siteName
