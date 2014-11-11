{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverlappingInstances
  , OverloadedStrings
  #-}
module Silk.Opaleye.Instances where

import Data.CaseInsensitive (CI)
import Data.Time
import System.Locale
import Data.Profunctor.Product.Default
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Opaleye.Internal.HaskellDB.Query (ShowConstant (..))
import Opaleye.RunQuery
import qualified Data.CaseInsensitive as CI
import qualified Data.Text            as ST
import qualified Data.Text.Lazy       as LT
import qualified Data.UUID            as UUID

instance ShowConstant UUID where
  showConstant = showConstant . UUID.toString

instance ShowConstant ST.Text where
  showConstant = showConstant . ST.unpack

instance ShowConstant LT.Text where
  showConstant = showConstant . LT.unpack

instance ShowConstant UTCTime where
  showConstant = showConstant . formatTime defaultTimeLocale format
    where
      format = "%Y-%m-%dT%H:%M:%SZ"

instance Default QueryRunnerColumn UUID    UUID    where def = fieldQueryRunnerColumn
instance Default QueryRunnerColumn ST.Text ST.Text where def = fieldQueryRunnerColumn
instance Default QueryRunnerColumn LT.Text LT.Text where def = fieldQueryRunnerColumn
instance Default QueryRunnerColumn UTCTime UTCTime where def = fieldQueryRunnerColumn

instance FromField (CI a) => Default QueryRunnerColumn (CI a) (CI a) where
  def = fieldQueryRunnerColumn

instance ShowConstant a => ShowConstant (CI a) where
  showConstant = showConstant . CI.original
