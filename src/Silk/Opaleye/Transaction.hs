{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , MultiParamTypeClasses
  , OverlappingInstances
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
-- | Composable transactions inside monad stacks.
--
-- TODO since Q has to be at the bottom (like IO typicall is) we may
-- be able to use MonadBaseControl to avoid re-wrapping stacks.
--
-- A function with a Transaction constraint can be composed with others to form a larger transaction.
-- A Database constraint indicates a complete transaction and can not be composed with others into a single transaction.
module Silk.Opaleye.Transaction where

import Control.Applicative
import Control.Exception
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Writer (WriterT)
import Data.Maybe
import Data.Monoid
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, defaultConnectInfo)
import System.Log.Logger
import qualified Database.PostgreSQL.Simple as PG

class (Functor m, Monad m) => Transaction m where
  liftQ :: Q a -> m a

instance Transaction Q where
  liftQ = id

instance (Transaction m, Error e) => Transaction (ErrorT e m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (ReaderT r m) where
  liftQ = lift . liftQ

instance (Monoid w, Transaction m) => Transaction (WriterT w m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (StateT s m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (IdentityT m) where
  liftQ = lift . liftQ

newtype Q a = Q { unQ :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadReader Connection)

unsafeIOToTransaction :: Transaction t => IO a -> t a
unsafeIOToTransaction = liftQ . unsafeIOToQ

unsafeIOToQ :: IO a -> Q a
unsafeIOToQ = Q . liftIO

runTransaction' :: Q a -> Pool Connection -> IO a
runTransaction' q p = runT q p (withRetry 1)
  where
    withRetry n act = act `catchRecoverableExceptions` handler n act
    handler n act (SomeException e) =
      if n < maxDbTries
        then do warningM "Db" ("Exception during database action, retrying: " ++ show e)
                withRetry (n + 1) act
        else throwIO e
    maxDbTries = 3 :: Int
    catchRecoverableExceptions :: IO a -> (SomeException -> IO a) -> IO a
    catchRecoverableExceptions action h = action `catches`
      [ Handler $ \(e :: AsyncException)            -> throwIO e
      , Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> throwIO e
      , Handler $ \(e :: BlockedIndefinitelyOnMVar) -> throwIO e
      , Handler $ \(e :: Deadlock)                  -> throwIO e
      , Handler $ \(e :: SomeException)             -> h e
      ]
    runT :: Q a -> Pool Connection -> (IO a -> IO a) -> IO a
    runT t pc catchExc = usePool pc $ \conn
      -> catchExc
       . PG.withTransaction conn
       . flip runReaderT conn
       . unQ $ t

class (Functor m, Monad m) => Database m where
  runTransaction :: Q a -> m a

instance Database m => Database (ReaderT r m) where
  runTransaction = lift . runTransaction

instance Database (ReaderT (Pool Connection) IO) where
  runTransaction t = ask >>= lift . runTransaction' t

instance (Error e, Database m) => Database (ErrorT e m) where
  runTransaction = lift . runTransaction

runPoolReader :: Config -> ReaderT (Pool Connection) IO b -> IO b
runPoolReader cfg q = do
  pool <- createPGPool cfg
  runReaderT q pool

usePool :: MonadIO m => Pool Connection -> (Connection -> IO a) -> m a
usePool p f = liftIO $ withResource p f

createPGPool :: Config -> IO (Pool Connection)
createPGPool config = createPool (connect' config) disconnect' 10 5 10
  where
    connect' :: Config -> IO Connection
    connect' x = PG.connect . connectInfo $ x
    disconnect' :: Connection -> IO ()
    disconnect' x = PG.close x

data Config = Config
  { name    :: String
  , user    :: String
  , pass    :: String
  , host    :: Maybe String
  , port    :: Maybe Int
  , traceId :: Maybe String
  } deriving Show

connectInfo :: Config -> ConnectInfo
connectInfo config =
  ConnectInfo
    { connectHost     = fromMaybe (connectHost defaultConnectInfo)          $ host config
    , connectPort     = maybe (connectPort defaultConnectInfo) fromIntegral $ port config
    , connectUser     = user config
    , connectPassword = pass config
    , connectDatabase = name config
    }
