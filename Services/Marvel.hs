{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Services.Marvel
  ( MarvelError(..)
  , PaginationOptions(..)
  , CharactersResponse(..)
  , ComicsResponse(..)
  , CharacterResponse(..)
  , ComicResponse(..)
  , FeaturedCharactersResponse(..)
  , FeaturedComicsResponse(..)
  , defaultPaginationOptions
  , findAllCharacters
  , findAllComics
  , findCharacter
  , findComic
  , fetchFeaturedCharacters
  , fetchFeaturedComics
  ) where

import BasicPrelude

import Control.Applicative (liftA)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), Value(Object), eitherDecode)
import Data.Aeson.Types ((.:))
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.Wreq (defaults, param, getWith, responseBody, statusCode)

import Config (ConfigM)
import qualified Config as Cfg
import Models.Character (Character)
import Models.Comic (Comic)
import qualified Models.Image as I
import Models.FeaturedCharacter (FeaturedCharacter)
import qualified Models.FeaturedCharacter as FCh
import Models.FeaturedComic (FeaturedComic)
import qualified Models.FeaturedComic as FCo
import Models.Pagination (Pagination)
import qualified Models.Pagination as P

data MarvelError = NotFound
                 | InvalidJson Text
                 deriving (Show)

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

data FeaturedComicsResponse = FeaturedComicsResponse
  { featuredComics :: [FeaturedComic]
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

-- Wrap `Data.Aeson.eitherDecode` to use our own custom error type
eitherErrorDecode :: FromJSON a => BL.ByteString -> Either MarvelError a
eitherErrorDecode byteString =
  case eitherDecode byteString of
    Left errDescription -> Left (InvalidJson (T.pack errDescription))
    Right result -> Right result

-- Use in all public functions to catch certain non-2xx API responses
handleHttpException :: ConfigM (Either MarvelError a) -> ConfigM (Either MarvelError a)
handleHttpException f = f `catch` httpExceptionHandler 

httpExceptionHandler :: HttpException -> ConfigM (Either MarvelError a)
httpExceptionHandler (StatusCodeException s _ _)
  | s ^. statusCode == 404 = return (Left NotFound)
httpExceptionHandler e = liftIO (throwIO e)

findAllCharacters :: PaginationOptions -> ConfigM (Either MarvelError CharactersResponse)
findAllCharacters paginationOptions = handleHttpException $ do
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
  let result = eitherErrorDecode (r ^. responseBody)
  return result

findAllComics :: PaginationOptions -> ConfigM (Either MarvelError ComicsResponse)
findAllComics paginationOptions = handleHttpException $ do
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
  let result = eitherErrorDecode (r ^. responseBody)
  return result

findCharacter :: Int -> ConfigM (Either MarvelError CharacterResponse)
findCharacter characterId = handleHttpException $ do
  publicKey <- asks Cfg.marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
  let url = "http://gateway.marvel.com/v1/public/characters/" ++ show characterId
  r <- (liftIO (getWith opts (T.unpack url)))
  let result = eitherErrorDecode (r ^. responseBody)
  return result

findComic :: Int -> ConfigM (Either MarvelError ComicResponse)
findComic comicId = handleHttpException $ do
  publicKey <- asks Cfg.marvelPublicKey
  ts <- liftIO getTimestamp
  apiHash <- createHash ts
  let opts = defaults & param "ts" .~ [show ts]
                      & param "apikey" .~ [publicKey]
                      & param "hash" .~ [apiHash]
  let url = "http://gateway.marvel.com/v1/public/comics/" ++ show comicId
  r <- liftIO (getWith opts (T.unpack url))
  let result = eitherErrorDecode (r ^. responseBody)
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

fetchFeaturedCharacters :: ConfigM (Either MarvelError FeaturedCharactersResponse)
fetchFeaturedCharacters =
  -- Endpoint doesn't exist, so fake it
  let response = FeaturedCharactersResponse
        { featuredCharacters=mockFeaturedCharacters }
  in return (Right response)

mockFeaturedComics :: [FeaturedComic]
mockFeaturedComics =
  [ FCo.FeaturedComic
    { FCo.id=57382
    , FCo.name="Black Panther (2016) #1"
    , FCo.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/c/80/567065cfebad5"
      , I.extension="jpg"
      }
    }
  , FCo.FeaturedComic
    { FCo.id=55398
    , FCo.name="Darth Vader (2015) #18"
    , FCo.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/1/a0/56f1836f99068"
      , I.extension="jpg"
      }
    }
  , FCo.FeaturedComic
    { FCo.id=55766
    , FCo.name="Star Wars (2015) #17"
    , FCo.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/9/b0/56e71aef71d19"
      , I.extension="jpg"
      }
    }
  , FCo.FeaturedComic
    { FCo.id=55360
    , FCo.name="All-New, All-Different Avengers (2015) #7"
    , FCo.thumbnail=I.Image
      { I.path="http://i.annihil.us/u/prod/marvel/i/mg/3/80/56e7074c31b84"
      , I.extension="jpg"
      }
    }
  ]

fetchFeaturedComics :: ConfigM (Either MarvelError FeaturedComicsResponse)
fetchFeaturedComics =
  -- Endpoint doesn't exist, so fake it
  let response = FeaturedComicsResponse
        { featuredComics=mockFeaturedComics }
  in return (Right response)
