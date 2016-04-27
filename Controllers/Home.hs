{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home (getHome) where

import qualified Data.Text.Lazy as TL
import Web.Scotty.Trans (ActionT, text)

import Config (ConfigM)

getHome :: ActionT TL.Text ConfigM ()
getHome = text "hello world"
