{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
-- | Json data stored in a postgres text column, this does not decode into a specific type.
module Silk.Opaleye.Json
  ( Json (..)
  , toJson
  , fromJson
  , makeJsonColumnInstances
  ) where

import Data.Aeson.Utils
import Language.Haskell.TH

import Silk.Opaleye.TH

newtype Json = Json { unJson :: Value }
  deriving (Eq, FromJSON, Show, ToJSON, Typeable)

mkJson :: Value -> Maybe Json
mkJson = Just . Json

makeColumnInstances ''Json ''Value 'unJson 'mkJson

toJson :: ToJSON a => a -> Json
toJson = Json . toJSON

fromJson :: FromJSON a => Json -> Maybe a
fromJson = (\case { Error _ -> Nothing; Success a -> Just a }) . fromJSON . unJson

makeJsonColumnInstances :: Name -> Q [Dec]
makeJsonColumnInstances n = makeColumnInstances n ''Json 'toJson 'fromJson
