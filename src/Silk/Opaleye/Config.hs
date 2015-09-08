{-# LANGUAGE Rank2Types #-}
module Silk.Opaleye.Config
  ( Config
  , Config_
  , Connection
  , ConnectInfo (..)
  , Pool
  , connectionPool
  , maxTries
  , onRetry
  , beforeTransaction
  , afterTransaction

  , makeConfig
  , defaultConfig
  , defaultPool
  , defaultBeforeTransaction
  , defaultOnRetry
  , defaultAfterTransaction

  , setCallbacks
  ) where

import Control.Exception (Exception)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import System.IO (hPutStrLn, stderr)
import qualified Database.PostgreSQL.Simple as PG

data Config a = Config
  { connectionPool    :: Pool Connection
      -- ^ Stores connections
  , maxTries          :: Int
      -- ^ Max number of times to catch an exception before propagating it.
  , beforeTransaction :: IO a
      -- ^ Code to run before transactions.
  , onRetry           :: forall e . Exception e => e -> a -> IO ()
      -- ^ Function to call when a retry occurs.
  , afterTransaction  :: a -> IO ()
      -- ^ Code to run after transactions, can be used with beforeTransaction to time transactions.
  }

-- | Shorthand for the default configuration where before- and afterTransaction don't communicate.
type Config_ = Config ()

makeConfig :: Pool Connection -> Config_
makeConfig pc = Config
  { connectionPool    = pc
  , maxTries          = 3
  , beforeTransaction = defaultBeforeTransaction
  , onRetry           = defaultOnRetry
  , afterTransaction  = defaultAfterTransaction
  }

-- | Constructs a config from login details.
defaultConfig :: ConnectInfo -> IO Config_
defaultConfig = fmap makeConfig . defaultPool

-- | Constructs a default connection pool.
defaultPool :: ConnectInfo -> IO (Pool Connection)
defaultPool connectInfo = createPool (PG.connect connectInfo) PG.close 10 5 10

-- | Default beforeTransaction hook, does nothing.
defaultBeforeTransaction :: IO ()
defaultBeforeTransaction = return ()

-- | Default onRetry hook, prints a warning.
defaultOnRetry :: Exception e => e -> a -> IO ()
defaultOnRetry e _ = hPutStrLn stderr $ "Warning: Exception during database action, retrying: " ++ show e

-- | Default afterTransaction hook, does nothing.
defaultAfterTransaction :: a -> IO ()
defaultAfterTransaction = const (return ())

-- A function to update all the callbacks at once.
setCallbacks :: IO a -> (forall e. Exception e => e -> a -> IO ()) -> (a -> IO ()) -> Config b -> Config a
setCallbacks before retry after c = c
  { beforeTransaction = before
  , onRetry           = retry
  , afterTransaction  = after
  }
