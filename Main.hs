{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import BasicPrelude

import Control.Applicative (liftA)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), eitherDecode)
import Data.Aeson.Types ((.:))
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param, getWith, responseBody)
import System.Environment (getEnv)

type App = ReaderT Config IO

data Config = Config
  { port :: Int
  , marvelPublicKey :: Text
  , marvelPrivateKey :: Text
  } deriving (Show)

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

getTimestamp :: IO Integer
getTimestamp = liftA round getPOSIXTime

createHash :: Integer -> App Text
createHash ts = do
  privateKey <- asks marvelPrivateKey
  publicKey <- asks marvelPublicKey
  let payload = show ts <> privateKey <> publicKey
  let apiHash = show (md5 (TLE.encodeUtf8 (TL.fromStrict payload)))
  return apiHash

findAllCharacters :: App (Either String CharactersResponse)
findAllCharacters = do
  publicKey <- asks marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let limit = 2 :: Int
  let offset = 0 :: Int
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
                      & param "limit" .~ [show limit]
                      & param "offset" .~ [show offset]
  r <- liftIO (getWith opts "http://gateway.marvel.com/v1/public/characters")
  let result = eitherDecode (r ^. responseBody)
  return result

application :: App ()
application = do
  result <- findAllCharacters
  liftIO (print (show result))

main :: IO ()
main = do
  config <- getConfig
  runReaderT application config
