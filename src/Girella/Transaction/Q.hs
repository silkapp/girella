{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , NoImplicitPrelude
  #-}
module Girella.Transaction.Q
  ( Q (..)
  , unsafeIOToQ
  ) where

import Prelude.Compat

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Database.PostgreSQL.Simple (Connection)

newtype Q a = Q { unQ :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadReader Connection)

unsafeIOToQ :: IO a -> Q a
unsafeIOToQ = Q . liftIO
