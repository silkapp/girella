{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , OverloadedStrings
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
import Data.String.Conversions
import Data.Text (Text)
import Language.Haskell.TH

import Silk.Opaleye.TH

newtype Json = Json { unJson :: Value }
  deriving (Eq, FromJSON, Show, ToJSON, Typeable)

jsonToText :: Json -> StrictText
jsonToText = cs . encode . unJson

textToJson :: StrictText -> Maybe Json
textToJson = decodeV . cs

makeColumnInstances ''Json ''Text 'jsonToText 'textToJson

toJson :: ToJSON a => a -> Json
toJson = Json . toJSON

fromJson :: FromJSON a => Json -> Maybe a
fromJson = (\case { Error _ -> Nothing; Success a -> Just a }) . fromJSON . unJson

makeJsonColumnInstances :: Name -> Q [Dec]
makeJsonColumnInstances n = makeColumnInstances n ''Json 'toJson 'fromJson
