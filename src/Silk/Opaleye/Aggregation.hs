{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  #-}
module Silk.Opaleye.Aggregation
  ( sum_
  , count
  , avg
  , max_
  , maxNullable
  , min_
  , minNullable
  , boolOr
  , boolAnd
--  , stringAgg
  , safeCoerceAgg
  -- * Renames
  , groupBy_
  -- * Re-exports
  , Aggregator
  , aggregate
  ) where

import Data.Int (Int64)
import Data.Profunctor (Profunctor, dimap)

import Opaleye.Aggregate (Aggregator, aggregate)
import Opaleye.Column (Column, Nullable)
import Opaleye.PGTypes (PGBool)
import qualified Opaleye.Aggregate as A (avg, boolAnd, boolOr, count, groupBy, max, min, sum)

import Silk.Opaleye.Compat (PGOrd)
import Silk.Opaleye.ShowConstant (PGRep, safeCoerceFromRep, safeCoerceToRep)

groupBy_ :: Aggregator (Column a) (Column a)
groupBy_ = A.groupBy

sum_ :: Aggregator (Column a) (Column a)
sum_ = safeCoerceAgg A.sum

count :: Aggregator (Column a) (Column Int64)
count = dimap id safeCoerceFromRep A.count

avg :: Aggregator (Column Double) (Column Double)
avg = safeCoerceAgg A.avg

max_ :: PGOrd (PGRep a) => Aggregator (Column a) (Column a)
max_ = safeCoerceAgg A.max

maxNullable :: Aggregator (Column (Nullable a)) (Column (Nullable a))
maxNullable = A.max

min_ :: PGOrd (PGRep a) => Aggregator (Column a) (Column a)
min_ = safeCoerceAgg A.min

minNullable :: Aggregator (Column (Nullable a)) (Column (Nullable a))
minNullable = A.min

boolOr :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolOr = safeCoerceAgg A.boolOr

boolAnd :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolAnd = safeCoerceAgg A.boolAnd

-- TODO Needs opaleye 0.4
--stringAgg :: PGRep a ~ PGText => Column a -> Aggregator (Column a) (Column a)
--stringAgg = safeCoerceAgg . A.stringAgg . safeCoerceToRep

-- TODO: Missing arrayAgg

safeCoerceAgg :: Profunctor p
  => p (Column (PGRep a)) (Column (PGRep b))
  -> p (Column        a ) (Column        b )
safeCoerceAgg = dimap safeCoerceToRep safeCoerceFromRep
