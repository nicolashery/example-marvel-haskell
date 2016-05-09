{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers.PathInfo (getRootPath) where

import BasicPrelude

import Network.Wai (Request, pathInfo)

getRootPath :: Request -> Text
getRootPath req =
  "/" ++ headOrEmptyString (pathInfo req)

headOrEmptyString :: [Text] -> Text
headOrEmptyString [] = ""
headOrEmptyString (x:_) = x
