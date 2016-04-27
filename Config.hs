{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
  ( Config(..)
  , ConfigM(..)
  , getConfig
  ) where

import BasicPrelude

import Control.Applicative (liftA)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Text as T
import System.Environment (getEnv)

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
