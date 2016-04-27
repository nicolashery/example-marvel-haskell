{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude

import Control.Monad.Reader (runReaderT)
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty.Trans (ScottyT, scottyT, get, middleware)

import Config (ConfigM, getConfig, runConfigM)
import qualified Config as Cfg
import Controllers.Home (getHome)
import Controllers.Characters (getCharacters)

routes :: ScottyT TL.Text ConfigM ()
routes = do
  get "/" getHome
  get "/characters" getCharacters

application :: ScottyT TL.Text ConfigM ()
application = do
  middleware logStdoutDev
  routes

main :: IO ()
main = do
  config <- getConfig
  let _port = Cfg.port config
  let runIO m = runReaderT (runConfigM m) config
  scottyT _port runIO application
