{-# OPTIONS -fno-warn-deprecations #-}
{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
-- | Composable transactions inside monad stacks.
--
-- TODO since Q has to be at the bottom (like IO typically is) we may
-- be able to use MonadBaseControl to avoid re-wrapping stacks.
--
-- A function with a Transaction constraint can be composed with others to form a larger transaction.
-- A MonadPool constraint indicates a complete transaction and can not be composed with others into a single transaction.
module Silk.Opaleye.Transaction where

import Control.Applicative
import Control.Exception
import Control.Monad.Error.Class (Error)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (ErrorT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Monoid
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import System.Log.Logger
import qualified Database.PostgreSQL.Simple as PG

import Silk.Opaleye.Transaction.Q (Q (..), unsafeIOToQ)

class (Functor m, Applicative m, Monad m) => Transaction m where
  liftQ :: Q a -> m a

instance Transaction Q where
  liftQ = id

instance (Transaction m, Error e) => Transaction (ErrorT e m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (ExceptT e m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (ReaderT r m) where
  liftQ = lift . liftQ

instance (Monoid w, Transaction m) => Transaction (WriterT w m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (StateT s m) where
  liftQ = lift . liftQ

instance Transaction m => Transaction (IdentityT m) where
  liftQ = lift . liftQ

unsafeIOToTransaction :: Transaction m => IO a -> m a
unsafeIOToTransaction = liftQ . unsafeIOToQ

runQ :: forall a . Q a -> Pool Connection -> IO a
runQ q p = withRetry 1 runT
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
    runT = withResource p $ \conn ->
      PG.withTransaction conn . flip runReaderT conn . unQ $ q

class (Functor m, Applicative m, Monad m) => MonadPool m where
  runTransaction :: Q a -> m a

instance MonadPool (ReaderT (Pool Connection) IO) where
  runTransaction t = ask >>= lift . runQ t

instance MonadPool m => MonadPool (ReaderT r m) where
  runTransaction = lift . runTransaction

instance (MonadPool m, Error e) => MonadPool (ErrorT e m) where
  runTransaction = lift . runTransaction

instance MonadPool m => MonadPool (ExceptT e m) where
  runTransaction = lift . runTransaction

instance MonadPool m => MonadPool (MaybeT m) where
  runTransaction = lift . runTransaction

createPGPool :: ConnectInfo -> IO (Pool Connection)
createPGPool connectInfo = createPool (PG.connect connectInfo) PG.close 10 5 10
