{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Config
  ( AppEnv(..)
  , Config(..)
  , ConfigM(..)
  , getConfig
  ) where

import BasicPrelude

import Control.Applicative (liftA)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Control
  ( liftBaseWith
  , MonadBaseControl
  , restoreM
  , RunInBase
  , StM
  )
import qualified Data.Text as T
import System.Environment (getEnv, lookupEnv)

data AppEnv = Development
            | Production
            deriving (Show, Read)

data Config = Config
  { env :: AppEnv
  , port :: Int
  , marvelPublicKey :: Text
  , marvelPrivateKey :: Text
  } deriving (Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader Config
             , MonadBase IO
             )

instance MonadBaseControl IO ConfigM where
  type StM ConfigM a = StM (ReaderT Config IO) a

  liftBaseWith :: forall a. (RunInBase ConfigM IO -> IO a) -> ConfigM a
  liftBaseWith f = ConfigM (liftBaseWith f')
    where
      f' :: RunInBase (ReaderT Config IO) IO -> IO a
      f' runInReaderBase = f runInConfigBase
        where
          -- type RunInBase m b = forall a. m a -> b (StM m a)
          runInConfigBase :: RunInBase ConfigM IO
          runInConfigBase = runInReaderBase . runConfigM

  restoreM :: StM ConfigM a -> ConfigM a
  restoreM = ConfigM . restoreM

getConfig :: IO Config
getConfig = do
  _env <- liftA read (liftA T.pack (getEnvDefault "Development" "APP_ENV"))
  _port <- liftA read (liftA T.pack (getEnv "PORT"))
  _marvelPublicKey <- liftA T.pack (getEnv "MARVEL_PUBLIC_KEY")
  _marvelPrivateKey <- liftA T.pack (getEnv "MARVEL_PRIVATE_KEY")
  return Config
    { env = _env
    , port = _port
    , marvelPublicKey = _marvelPublicKey
    , marvelPrivateKey = _marvelPrivateKey
    }

getEnvDefault :: String -> String -> IO String
getEnvDefault defaultValue envVar =
  liftA (fromMaybe defaultValue) (lookupEnv envVar)
