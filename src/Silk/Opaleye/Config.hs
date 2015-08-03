{-# LANGUAGE Rank2Types #-}
module Silk.Opaleye.Config
  ( Config
  , connectionPool
  , maxTries
  , onRetry
  , beforeTransaction
  , afterTransaction

  , makeConfig
  , defaultConfig
  , defaultBeforeTransaction
  , defaultAfterTransaction

  , defaultPool
  ) where

import Control.Exception (Exception)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import System.IO (hPutStrLn, stderr)
import qualified Database.PostgreSQL.Simple as PG

data Config a = Config
  { connectionPool    :: Pool Connection
  , maxTries          :: Int
  , beforeTransaction :: IO a
  , onRetry           :: forall e . Exception e => e -> a -> IO ()
  , afterTransaction  :: a -> IO ()
  }

type Config_ = Config ()

makeConfig :: Pool Connection -> Config_
makeConfig pc = Config
  { connectionPool    = pc
  , maxTries          = 3
  , beforeTransaction = defaultBeforeTransaction
  , onRetry           = defaultOnRetry
  , afterTransaction  = defaultAfterTransaction
  }

defaultConfig :: ConnectInfo -> IO (Config ())
defaultConfig = fmap makeConfig . defaultPool

defaultPool :: ConnectInfo -> IO (Pool Connection)
defaultPool connectInfo = createPool (PG.connect connectInfo) PG.close 10 5 10

defaultOnRetry :: Exception e => e -> a -> IO ()
defaultOnRetry e _ = hPutStrLn stderr $ "Warning: Exception during database action, retrying: " ++ show e

defaultBeforeTransaction :: IO ()
defaultBeforeTransaction = return ()

defaultAfterTransaction :: a -> IO ()
defaultAfterTransaction = const (return ())
