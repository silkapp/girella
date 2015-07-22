{-# LANGUAGE Rank2Types #-}
module Silk.Opaleye.Config
  ( Config
  , connectionPool
  , maxTries
  , onRetry

  , makeConfig
  , defaultConfig

  , defaultPool
  ) where

import Control.Exception (Exception)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import System.IO (hPutStrLn, stderr)
import qualified Database.PostgreSQL.Simple as PG

data Config = Config
  { connectionPool :: Pool Connection
  , maxTries       :: Int
  , onRetry        :: Exception e => e -> IO ()
  }

makeConfig :: Pool Connection -> Config
makeConfig pc = Config
  { connectionPool = pc
  , maxTries       = 3
  , onRetry        = defaultOnRetry
  }

defaultConfig :: ConnectInfo -> IO Config
defaultConfig = fmap makeConfig . defaultPool

defaultPool :: ConnectInfo -> IO (Pool Connection)
defaultPool connectInfo = createPool (PG.connect connectInfo) PG.close 10 5 10

defaultOnRetry :: Exception e => e -> IO ()
defaultOnRetry e = hPutStrLn stderr $ "Warning: Exception during database action, retrying: " ++ show e
