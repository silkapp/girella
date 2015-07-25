{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
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

import Silk.Opaleye.Compat (unsafeCoerceColumn)
import Silk.Opaleye.ShowConstant

boolOr :: Aggregator (Column Bool) (Column Bool)
boolOr = dimap unsafeCoerceColumn unsafeCoerceColumn A.boolOr

boolAnd :: Aggregator (Column Bool) (Column Bool)
boolAnd = dimap unsafeCoerceColumn unsafeCoerceColumn A.boolAnd

maxAggr :: forall a. TOrd a => Aggregator (Column a) (Column a)
maxAggr = dimap toRep fromRep A.max
  where
    toRep :: Column a -> Column (OrdRep a)
    toRep = unsafeCoerceColumn
    fromRep :: Column (OrdRep a) -> Column a
    fromRep = unsafeCoerceColumn

minAggr :: forall a. TOrd a => Aggregator (Column a) (Column a)
minAggr = dimap toRep fromRep A.min
  where
    toRep :: Column a -> Column (OrdRep a)
    toRep = unsafeCoerceColumn
    fromRep :: Column (OrdRep a) -> Column a
    fromRep = unsafeCoerceColumn
