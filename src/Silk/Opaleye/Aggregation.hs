{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  #-}
module Silk.Opaleye.Aggregation
  ( Aggregator
  , aggregate
  , boolAnd
  , boolOr
  , count
  , groupBy
  , maxAggr
  , minAggr
  ) where

import Data.Profunctor (Profunctor, dimap)

import Opaleye.Aggregate (Aggregator, aggregate, count, groupBy)
import Opaleye.Column (Column)
import Opaleye.PGTypes (PGBool)
import qualified Opaleye.Aggregate as A (boolAnd, boolOr, max, min)

import Silk.Opaleye.ShowConstant

boolOr :: (ShowConstant a, PGRep a ~ PGBool) => Aggregator (Column a) (Column a)
boolOr = safeAgg A.boolOr

boolAnd :: (ShowConstant a, PGRep a ~ PGBool) => Aggregator (Column a) (Column a)
boolAnd = safeAgg A.boolAnd

maxAggr :: (ShowConstant a, PGOrd (PGRep a)) => Aggregator (Column a) (Column a)
maxAggr = safeAgg A.max

minAggr :: (ShowConstant a, PGOrd (PGRep a)) => Aggregator (Column a) (Column a)
minAggr = safeAgg A.min

safeAgg :: Profunctor p
  => p (Column (PGRep a)) (Column (PGRep b))
  -> p (Column        a ) (Column        b )
safeAgg = dimap safeCoerceToRep safeCoerceFromRep
