{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude

import Control.Monad.Reader (runReaderT)
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Web.Scotty.Trans (ScottyT, scottyT, get, middleware)

import Config (ConfigM, getConfig, runConfigM)
import qualified Config as Cfg
import Controllers.Home (getHome)
import Controllers.Characters (getCharacters)
import Controllers.Comics (getComics)

router :: ScottyT TL.Text ConfigM ()
router = do
  get "/" getHome
  get "/characters" getCharacters
  get "/comics" getComics

application :: ScottyT TL.Text ConfigM ()
application = do
  middleware (staticPolicy (noDots >-> addBase "static"))
  middleware logStdoutDev
  router

main :: IO ()
main = do
  config <- getConfig
  let _port = Cfg.port config
  let runIO m = runReaderT (runConfigM m) config
  scottyT _port runIO application
