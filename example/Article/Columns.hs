{-# LANGUAGE
    DeriveDataTypeable
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module Article.Columns (Id (..)) where

import Data.Typeable (Typeable)
import Data.UUID

import Girella.TH

-- | We use a newtype to reperesent a foreign key
newtype Id = Id { unId :: UUID }
  deriving (Show, Typeable)

mkId ''Id
