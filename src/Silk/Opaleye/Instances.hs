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
import Data.Profunctor.Product.Default
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Opaleye.Internal.HaskellDB.Query (ShowConstant (..))
import Opaleye.RunQuery
import qualified Data.CaseInsensitive as CI

instance FromField (CI a) => Default QueryRunnerColumn (CI a) (CI a) where
  def = fieldQueryRunnerColumn

instance ShowConstant a => ShowConstant (CI a) where
  showConstant = showConstant . CI.original
