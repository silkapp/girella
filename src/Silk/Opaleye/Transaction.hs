{-# OPTIONS -fno-warn-deprecations #-}
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , NoImplicitPrelude
  , OverloadedStrings
  , ScopedTypeVariables
  #-}
-- | Composable transactions inside monad stacks.
--
-- TODO since Q has to be at the bottom (like IO typically is) we may
-- be able to use MonadBaseControl to avoid re-wrapping stacks.
--
-- A function with a Transaction constraint can be composed with others to form a larger transaction.
-- A MonadPool constraint indicates a complete transaction and can not be composed with others into a single transaction.
module Silk.Opaleye.Transaction
  ( Transaction (..)
  , MonadPool (..)
  , Q
  , defaultRunTransaction
  , unsafeIOToTransaction
  ) where

import Prelude.Compat

import Control.Exception (AsyncException, BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM,
                          Deadlock, Handler (Handler), SomeException (SomeException), catches,
                          throwIO)
import Control.Monad.Error.Class (Error)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO (..), lift)
import Control.Monad.Trans.Error (ErrorT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Pool (withResource)
import qualified Database.PostgreSQL.Simple as PG

import Silk.Opaleye.Config
import Silk.Opaleye.Transaction.Q (Q (..), unsafeIOToQ)

-- | Composable transactions, this is the layer where queries are
-- combined. Typically all sequences of queries should have a
-- Transaction constraint rather than MonadPool (see below) since the
-- end user can then decide whether to combine different Transactions
-- into one. There is no IO (but see 'unsafeIOToTransaction') since
-- database transactions can't be combined with arbitrary other
-- effects.
--
-- You may se the type 'Q' instead of a 'Transaction' constraint
-- sometimes, they have the same relationship as 'IO' and 'MonadIO'
-- do.
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

instance Transaction m => Transaction (MaybeT m) where
  liftQ = lift . liftQ

-- High level composition of database transactions with configuration
-- to run them. You can call 'runTransaction' to execute
-- Transactions. Several Transaction's can be combined into one, or
-- other IO-related operations can be interleaved between them. If You
-- only combine queries, just use Transaction instead of MonadPool.
class (Functor m, Applicative m, Monad m) => MonadPool m where
  runTransaction :: Q a -> m a

instance {-# OVERLAPPABLE #-} MonadPool m => MonadPool (ReaderT r m) where
  runTransaction = lift . runTransaction

instance (MonadPool m, Error e) => MonadPool (ErrorT e m) where
  runTransaction = lift . runTransaction

instance MonadPool m => MonadPool (ExceptT e m) where
  runTransaction = lift . runTransaction

instance MonadPool m => MonadPool (MaybeT m) where
  runTransaction = lift . runTransaction

instance MonadPool m => MonadPool (StateT s m) where
  runTransaction = lift . runTransaction

instance {-# OVERLAPPING #-} MonadPool (ReaderT (Config a) IO) where
  runTransaction = defaultRunTransaction ask

-- | Run an arbitrary IO computation inside a Transaction. I hope you
-- know what you are doing.
unsafeIOToTransaction :: Transaction m => IO a -> m a
unsafeIOToTransaction = liftQ . unsafeIOToQ

-- | The default way to run a transaction or implement MonadPool.
-- With a typical ReaderT over some data type containing a 'Config c'
-- you can implement 'MonadPool''s 'runTransaction' as
--
-- > defaultRunTransaction (asks myConfig)
--
-- It catches "recoverable" exceptions and retries according to the 'Config'.
defaultRunTransaction :: forall m c a . MonadIO m => m (Config c) -> Q a -> m a
defaultRunTransaction mc t = liftIO . run t =<< mc
  where
    run :: Q a -> Config c -> IO a
    run q cfg = do
      c <- beforeTransaction cfg
      res <- withRetry c 1
        $ withResource (connectionPool cfg)
        $ \conn -> PG.withTransaction conn . flip runReaderT conn . unQ $ q
      afterTransaction cfg c
      return res
      where
        withRetry :: c -> Int -> IO a -> IO a
        withRetry c n act = act `catchRecoverableExceptions` handler c n act
        handler :: c -> Int -> IO a -> SomeException -> IO a
        handler c n act (SomeException e) =
          if n < maxTries cfg
            then onRetry cfg e c >> withRetry c (n + 1) act
            else throwIO e
        catchRecoverableExceptions :: IO a -> (SomeException -> IO a) -> IO a
        catchRecoverableExceptions action h = action `catches`
          [ Handler $ \(e :: AsyncException)            -> throwIO e
          , Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> throwIO e
          , Handler $ \(e :: BlockedIndefinitelyOnMVar) -> throwIO e
          , Handler $ \(e :: Deadlock)                  -> throwIO e
          , Handler $ \(e :: SomeException)             -> h e
          ]
