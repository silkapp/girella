{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , InstanceSigs
  , NoImplicitPrelude
  , ScopedTypeVariables
  #-}
module Main (main) where

import Prelude.Compat

import Control.Monad.Reader
import Data.UUID
import qualified Data.UUID.V4 as UUID (nextRandom)

import Silk.Opaleye
import qualified User

-- | Set up this example with
-- > create database test;
-- > create user test with password '1234';
main :: IO ()
main = do
  -- You can create a config by only providing 'ConnectInfo' to
  -- 'defaultConfig' which will generate a default connection pool. If
  -- you want to configure the pool use 'makeConfig'.
  --
  -- The default setting type is 'Config_' = 'Config ()', See
  -- 'Girella.Config' for more info about the type argument.
  cfg :: Config_
      <- defaultConfig ConnectInfo
           { connectHost     = "localhost"
           , connectPort     = 5432
           , connectUser     = "test"
           , connectPassword = "1234"
           , connectDatabase = "test"
           }

  -- Create your own type or use a 'ReaderT' to store the 'Config'.
  runMyTransStack cfg doThings
  runInReaderT    cfg doThings

-- | Normal runner for a transformer.
-- 'Config_' = 'Config ()'. See 'Girella.Config' for customization.
runMyTransStack :: Config_ -> MyTransStack a -> IO a
runMyTransStack cfg = flip runReaderT cfg . unTransStack

-- | Runs our queries in just a 'ReaderT' instead.
runInReaderT :: Config c -> ReaderT (Config c) IO a -> IO a
runInReaderT = flip runReaderT

-- | 'MonadPool' denotes an environment that can run transactions of
-- queries. Transactions in MonadPool's can't be combined, they will
-- run separately.
doThings :: (MonadPool m, MonadIO m) => m ()
doThings = do
  i <- liftIO UUID.nextRandom
  people <- runTransaction $ myTransaction i
  liftIO $ print people

-- | A 'Transaction' form just that, a database Transaction. Note that
-- we don't run the transaction here, which is why you can combine
-- multiple transactions into one.
--
-- By combining these queries here we prevent them from running in isolation of each other.
myTransaction :: Transaction m => UUID -> m [User.UserH]
myTransaction i = do
  User.insert i "Aaron Aardvark" 12 (Just User.Male)
  runQuery User.allByName

-- | The Transformer stack, we need to stuff a Config somewhere in
-- there to run queries.
newtype MyTransStack a = MyTransStack { unTransStack :: ReaderT Config_ IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader Config_
    )

-- | Defines how to run transactions in our stack, typically by using
-- 'defaultRunTransaction' which needs a way to get the 'Config' out
-- of our stack.
instance MonadPool MyTransStack where
  runTransaction :: Q a -> MyTransStack a
  runTransaction = defaultRunTransaction getConfig
    where
      getConfig :: MyTransStack Config_
      getConfig = ask
