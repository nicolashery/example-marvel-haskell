{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Services.Marvel
  ( PaginationOptions(..)
  , CharactersResponse(..)
  , ComicsResponse(..)
  , CharacterResponse(..)
  , ComicResponse(..)
  , FeaturedCharactersResponse(..)
  , defaultPaginationOptions
  , findAllCharacters
  , findAllComics
  , findCharacter
  , findComic
  , fetchFeaturedCharacters
  ) where

import BasicPrelude

import Control.Applicative (liftA)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), Value(Object), eitherDecode)
import Data.Aeson.Types ((.:))
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wreq (defaults, param, getWith, responseBody)

import Config (ConfigM)
import qualified Config as Cfg
import Models.Character (Character)
import Models.Comic (Comic)
import qualified Models.Image as I
import Models.FeaturedCharacter (FeaturedCharacter)
import qualified Models.FeaturedCharacter as FCh
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

data CharacterResponse = CharacterResponse
  { character :: Character
  } deriving (Show)

instance FromJSON CharacterResponse where
  parseJSON (Object o) = do
    _data <- o .: "data"
    _results :: [Character] <- _data .: "results"
    case _results of
      [] -> fail "expected 'data.results' to be a non-empty list"
      (x:_) -> return CharacterResponse { character=x }
  parseJSON _ = mzero

data ComicResponse = ComicResponse
  { comic :: Comic
  } deriving (Show)

instance FromJSON ComicResponse where
  parseJSON (Object o) = do
    _data <- o .: "data"
    _results :: [Comic] <- _data .: "results"
    case _results of
      [] -> fail "expected 'data.results' to be a non-empty list"
      (x:_) -> return ComicResponse { comic=x }
  parseJSON _ = mzero

data FeaturedCharactersResponse = FeaturedCharactersResponse
  { featuredCharacters :: [FeaturedCharacter]
  } deriving (Show)

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

findCharacter :: Int -> ConfigM (Either String CharacterResponse)
findCharacter characterId = do
  publicKey <- asks Cfg.marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
  let url = "http://gateway.marvel.com/v1/public/characters/" ++ show characterId
  r <- liftIO (getWith opts (T.unpack url))
  let result = eitherDecode (r ^. responseBody)
  return result

findComic :: Int -> ConfigM (Either String ComicResponse)
findComic comicId = do
  publicKey <- asks Cfg.marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
  let url = "http://gateway.marvel.com/v1/public/comics/" ++ show comicId
  r <- liftIO (getWith opts (T.unpack url))
  let result = eitherDecode (r ^. responseBody)
  return result

mockFeaturedCharacters :: [FeaturedCharacter]
mockFeaturedCharacters =
  [ FCh.FeaturedCharacter
    { FCh.id=1009610
    , FCh.name="Spider-Man"
    , FCh.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/3/50/526548a343e4b"
      , I.extension="jpg"
      }
    }
  , FCh.FeaturedCharacter
    { FCh.id=1010338
    , FCh.name="Captain Marvel (Carol Danvers)"
    , FCh.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/6/80/5269608c1be7a"
      , I.extension="jpg"
      }
    }
  , FCh.FeaturedCharacter
    { FCh.id=1009351
    , FCh.name="Hulk"
    , FCh.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/5/a0/538615ca33ab0"
      , I.extension="jpg"
      }
    }
  , FCh.FeaturedCharacter
    { FCh.id=1009189
    , FCh.name="Black Widow"
    , FCh.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/f/30/50fecad1f395b"
      , I.extension="jpg"
      }
    }
  ]

fetchFeaturedCharacters :: ConfigM (Either String FeaturedCharactersResponse)
fetchFeaturedCharacters =
  -- Endpoint doesn't exist, so fake it
  let response = FeaturedCharactersResponse
        { featuredCharacters=mockFeaturedCharacters }
  in return (Right response)
