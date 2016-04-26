{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import BasicPrelude

import Control.Applicative (liftA)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), eitherDecode)
import Data.Aeson.Types ((.:))
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wreq (defaults, param, getWith, responseBody)
import System.Environment (getEnv)
import Web.Scotty.Trans (ScottyT, ActionT, scottyT, get, text, middleware)

data Config = Config
  { port :: Int
  , marvelPublicKey :: Text
  , marvelPrivateKey :: Text
  } deriving (Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = do
  _port <- liftA read (liftA T.pack (getEnv "PORT"))
  _marvelPublicKey <- liftA T.pack (getEnv "MARVEL_PUBLIC_KEY")
  _marvelPrivateKey <- liftA T.pack (getEnv "MARVEL_PRIVATE_KEY")
  return Config
    { port = _port
    , marvelPublicKey = _marvelPublicKey
    , marvelPrivateKey = _marvelPrivateKey
    }

data CharactersResponse = CharactersResponse
  { characters :: [Character]
  } deriving (Show)

instance FromJSON CharactersResponse where
  parseJSON (Object o) = do
    _data <- o .: "data"
    _characters <- _data .: "results"
    return (CharactersResponse _characters)
  parseJSON _ = mzero

data Character = Character
  { id :: Int
  , name :: Text
  , description :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Character
instance ToJSON Character

data PaginationOptions = PaginationOptions
  { limit :: Int
  , offset :: Int
  } deriving (Show)

defaultPaginationOptions :: PaginationOptions
defaultPaginationOptions = PaginationOptions
  { limit = 20
  , offset = 0
  }

getTimestamp :: IO Integer
getTimestamp = liftA round getPOSIXTime

createHash :: Integer -> ConfigM Text
createHash ts = do
  privateKey <- asks marvelPrivateKey
  publicKey <- asks marvelPublicKey
  let payload = show ts <> privateKey <> publicKey
  let apiHash = show (md5 (TLE.encodeUtf8 (TL.fromStrict payload)))
  return apiHash

findAllCharacters :: PaginationOptions -> ConfigM (Either String CharactersResponse)
findAllCharacters paginationOptions = do
  publicKey <- asks marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let _limit = limit paginationOptions
  let _offset = offset paginationOptions
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
                      & param "limit" .~ [show _limit]
                      & param "offset" .~ [show _offset]
  r <- liftIO (getWith opts "http://gateway.marvel.com/v1/public/characters")
  let result = eitherDecode (r ^. responseBody)
  return result

application :: ScottyT TL.Text ConfigM ()
application = do
  middleware logStdoutDev
  get "/" getHome
  get "/characters" getCharacters

getHome :: ActionT TL.Text ConfigM ()
getHome = text "hello world"

getCharacters :: ActionT TL.Text ConfigM ()
getCharacters = do
  result <- lift (findAllCharacters defaultPaginationOptions)
  text (TL.fromStrict (show result))

main :: IO ()
main = do
  config <- getConfig
  let _port = port config
  let runIO m = runReaderT (runConfigM m) config
  scottyT _port runIO application
