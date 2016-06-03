{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude

import Control.Monad.Reader (runReaderT)
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Web.Scotty.Trans (ScottyT, scottyT, get, middleware)

import Config (ConfigM, AppEnv(Production), getConfig, runConfigM)
import qualified Config as Cfg
import Controllers.Home (getHome)
import Controllers.Character (getCharacters, getCharacter)
import Controllers.Comic (getComics, getComic)
import Routes (Route(..), toPattern)

router :: ScottyT TL.Text ConfigM ()
router = do
  get (toPattern HomeRoute) getHome
  get (toPattern CharactersRoute) getCharacters
  get (toPattern CharacterRoute) getCharacter
  get (toPattern ComicsRoute) getComics
  get (toPattern ComicRoute) getComic

application :: AppEnv -> ScottyT TL.Text ConfigM ()
application env = do
  middleware (staticPolicy (noDots >-> addBase "static"))
  let logMiddleware = case env of Production -> logStdout
                                  _ -> logStdoutDev
  middleware logMiddleware
  router

main :: IO ()
main = do
  config <- getConfig
  let _env = Cfg.env config
  let _port = Cfg.port config
  let runIO m = runReaderT (runConfigM m) config
  scottyT _port runIO (application _env)
