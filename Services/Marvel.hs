{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Marvel
  ( PaginationOptions(..)
  , CharactersResponse(..)
  , ComicsResponse(..)
  , defaultPaginationOptions
  , findAllCharacters
  , findAllComics
  ) where

import BasicPrelude

import Control.Applicative (liftA)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), Value(Object), eitherDecode)
import Data.Aeson.Types ((.:))
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wreq (defaults, param, getWith, responseBody)

import Config (ConfigM)
import qualified Config as Cfg
import Models.Character (Character)
import Models.Comic (Comic)
import Models.Pagination (Pagination)
import qualified Models.Pagination as P

data CharactersResponse = CharactersResponse
  { characters :: [Character]
  , charactersPagination :: Pagination
  } deriving (Show)

instance FromJSON CharactersResponse where
  parseJSON (Object o) = do
    _data <- o .: "data"
    _offset <- _data .: "offset"
    _limit <- _data .: "limit"
    _total <- _data .: "total"
    _count <- _data .: "count"
    let _pagination = P.Pagination {
        P.offset=_offset
      , P.limit=_limit
      , P.total=_total
      , P.count=_count
      }
    _characters <- _data .: "results"
    return CharactersResponse
      { characters=_characters
      , charactersPagination=_pagination
      }
  parseJSON _ = mzero

data ComicsResponse = ComicsResponse
  { comics :: [Comic]
  , comicsPagination :: Pagination
  } deriving (Show)

instance FromJSON ComicsResponse where
  parseJSON (Object o) = do
    _data <- o .: "data"
    _offset <- _data .: "offset"
    _limit <- _data .: "limit"
    _total <- _data .: "total"
    _count <- _data .: "count"
    let _pagination = P.Pagination {
        P.offset=_offset
      , P.limit=_limit
      , P.total=_total
      , P.count=_count
      }
    _comics <- _data .: "results"
    return ComicsResponse
      { comics=_comics
      , comicsPagination=_pagination
      }
  parseJSON _ = mzero

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
  privateKey <- asks Cfg.marvelPrivateKey
  publicKey <- asks Cfg.marvelPublicKey
  let payload = show ts <> privateKey <> publicKey
  let apiHash = show (md5 (TLE.encodeUtf8 (TL.fromStrict payload)))
  return apiHash

findAllCharacters :: PaginationOptions -> ConfigM (Either String CharactersResponse)
findAllCharacters paginationOptions = do
  publicKey <- asks Cfg.marvelPublicKey
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

findAllComics :: PaginationOptions -> ConfigM (Either String ComicsResponse)
findAllComics paginationOptions = do
  publicKey <- asks Cfg.marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let _limit = limit paginationOptions
  let _offset = offset paginationOptions
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
                      & param "limit" .~ [show _limit]
                      & param "offset" .~ [show _offset]
  r <- liftIO (getWith opts "http://gateway.marvel.com/v1/public/comics")
  let result = eitherDecode (r ^. responseBody)
  return result

