{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}
module Main (main) where

import Prelude.Compat

import Control.Monad.Reader
import qualified Data.UUID.V4 as UUID (nextRandom)

import Silk.Opaleye
import qualified Example

-- | Set up this example with
-- > create database test;
-- > create user test with password '1234';
main :: IO ()
main = do
  -- You can create a config by only providing 'ConnectInfo' to
  -- 'defaultConfig' which will generate a default connection pool. If
  -- you want to configure the pool use 'makeConfig'.
  cfg <- defaultConfig $ ConnectInfo
           { connectHost     = "localhost"
           , connectPort     = 5432
           , connectUser     = "test"
           , connectPassword = "1234"
           , connectDatabase = "test"
           }
  flip runReaderT cfg $ unTransStack queries

queries :: MyTransStack ()
queries = do
  i <- liftIO UUID.nextRandom
  people :: [Example.PersonH] <- runTransaction $ do
    Example.insert i "Aaron Aardvark" 12 (Just Example.Male)
    runQuery Example.allByName
  liftIO $ print people

newtype MyTransStack a = MyTransStack { unTransStack :: ReaderT Config_ IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader Config_
    )

instance MonadPool MyTransStack where
  runTransaction = defaultRunTransaction ask
