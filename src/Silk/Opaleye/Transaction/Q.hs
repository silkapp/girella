{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  #-}
module Silk.Opaleye.Transaction.Q
  ( Q (..)
  , unsafeIOToQ
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Database.PostgreSQL.Simple (Connection)

newtype Q a = Q { unQ :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadReader Connection)

unsafeIOToQ :: IO a -> Q a
unsafeIOToQ = Q . liftIO
