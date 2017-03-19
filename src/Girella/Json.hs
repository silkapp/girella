{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
-- | Json data stored in a postgres text column, this does not decode into a specific type.
module Girella.Json
  ( Json (..)
  , toJson
  , fromJson
  , makeJsonColumnInstances
  ) where

import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value, decode, encode, fromJSON)
import Data.String.Conversions (StrictText, cs)
import Data.Typeable (Typeable)
import Language.Haskell.TH (Dec, Name, Q)

import Girella.TH

newtype Json = Json { unJson :: Value }
  deriving (Eq, FromJSON, Show, ToJSON, Typeable)

jsonToText :: Json -> StrictText
jsonToText = cs . encode . unJson

textToJson :: StrictText -> Maybe Json
textToJson = decode . cs

makeColumnInstances ''Json ''StrictText 'jsonToText 'textToJson

toJson :: ToJSON a => a -> Json
toJson = Json . toJSON

fromJson :: FromJSON a => Json -> Maybe a
fromJson = (\case { Error _ -> Nothing; Success a -> Just a }) . fromJSON . unJson

makeJsonColumnInstances :: Name -> Q [Dec]
makeJsonColumnInstances n = makeColumnInstances n ''Json 'toJson 'fromJson
