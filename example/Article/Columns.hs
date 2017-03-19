{-# LANGUAGE
    DeriveDataTypeable
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module Article.Columns
  ( Id (..)
  , Title (..)
  , Content (..)
  ) where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID

import Girella.TH

-- | We use a newtype to reperesent a foreign key
newtype Id = Id { unId :: UUID }
  deriving (Show, Typeable)

mkId ''Id

newtype Title = Title { unTitle :: Text }
  deriving Show

mkId ''Title

newtype Content = Content { unContent :: Text }
  deriving Show

mkId ''Content
