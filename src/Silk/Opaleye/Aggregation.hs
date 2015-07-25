{-# LANGUAGE
    FlexibleContexts
  , NoMonomorphismRestriction
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

import Data.Profunctor (dimap)

import Opaleye.Aggregate (Aggregator, aggregate, count, groupBy)
import Opaleye.Column (Column)
import qualified Opaleye.Aggregate as A (boolAnd, boolOr, max, min)

import Silk.Opaleye.ShowConstant

boolOr :: Aggregator (Column Bool) (Column Bool)
boolOr = dimap safeCoerceToRep safeCoerceFromRep A.boolOr

boolAnd :: Aggregator (Column Bool) (Column Bool)
boolAnd = dimap safeCoerceToRep safeCoerceFromRep A.boolAnd

maxAggr :: PGOrd a => Aggregator (Column a) (Column a)
maxAggr = A.max

minAggr :: PGOrd a => Aggregator (Column a) (Column a)
minAggr = A.min
