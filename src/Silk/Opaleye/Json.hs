{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  #-}
-- | Json data stored in a postgres text column, this does not decode into a specific type.
module Silk.Opaleye.Json where

import Data.Aeson.Utils
import Data.String.Conversions

import Silk.Opaleye.Instances ()
import Silk.Opaleye.TH

newtype Json = Json { unJson :: Value }
  deriving (Eq, FromJSON, Show, ToJSON, Typeable)

jsonToText :: Json -> StrictText
jsonToText = cs . encode . unJson

textToJson :: StrictText -> Maybe Json
textToJson = decodeV . cs

makeColumnInstances ''Json 'jsonToText 'textToJson
