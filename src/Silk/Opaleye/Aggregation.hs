{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  #-}
module Silk.Opaleye.Aggregation
  ( sum_
  , sumGrouped
  , count
  , avg
  , avgGrouped
  , max_
  , maxGrouped
  , maxNullable
  , min_
  , minGrouped
  , minNullable
  , boolOr
  , boolOrGrouped
  , boolAnd
  , boolAndGrouped
--  , stringAgg
  , safeCoerceAgg
  -- * Renames
  , groupBy_
  -- * Re-exports
  , Aggregator
  , aggregate
  ) where

import Data.Int (Int64)
import Data.Profunctor (Profunctor, dimap, rmap)

import Opaleye.Aggregate (Aggregator, aggregate)
import Opaleye.Column (Column, Nullable, toNullable)
import Opaleye.PGTypes (PGBool)
import qualified Opaleye.Aggregate as A (avg, boolAnd, boolOr, count, groupBy, max, min, sum)

import Silk.Opaleye.Compat (PGOrd)
import Silk.Opaleye.ShowConstant (PGRep, ShowConstant, safeCoerceFromRep, safeCoerceToRep)

groupBy_ :: Aggregator (Column a) (Column a)
groupBy_ = A.groupBy

-- TODO Constrain to numeric types.
-- http://www.postgresql.org/docs/9.4/static/functions-aggregate.html
sumGrouped :: ShowConstant a => Aggregator (Column a) (Column a)
sumGrouped = safeCoerceAgg A.sum

sum_ :: ShowConstant a => Aggregator (Column a) (Column (Nullable a))
sum_ = toNullable <$> sumGrouped

count :: ShowConstant a => Aggregator (Column a) (Column Int64)
count = rmap safeCoerceFromRep A.count

-- TODO Expand to numeric input (even integral)
-- http://www.postgresql.org/docs/9.4/static/functions-aggregate.html
avgGrouped :: Aggregator (Column Double) (Column Double)
avgGrouped = safeCoerceAgg A.avg

avg :: Aggregator (Column Double) (Column (Nullable Double))
avg = toNullable <$> avgGrouped

maxGrouped :: PGOrd (PGRep a) => Aggregator (Column a) (Column a)
maxGrouped = safeCoerceAgg A.max

max_ :: PGOrd (PGRep a) => Aggregator (Column a) (Column (Nullable a))
max_ = toNullable <$> maxGrouped

maxNullable :: PGOrd (PGRep a) => Aggregator (Column (Nullable a)) (Column (Nullable a))
maxNullable = A.max

minGrouped :: PGOrd (PGRep a) => Aggregator (Column a) (Column a)
minGrouped = safeCoerceAgg A.min

min_ :: PGOrd (PGRep a) => Aggregator (Column a) (Column (Nullable a))
min_ = toNullable <$> minGrouped

minNullable :: PGOrd (PGRep a) => Aggregator (Column (Nullable a)) (Column (Nullable a))
minNullable = A.min

boolOrGrouped :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolOrGrouped = safeCoerceAgg A.boolOr

boolOr :: PGRep a ~ PGBool => Aggregator (Column a) (Column (Nullable a))
boolOr = toNullable <$> boolOrGrouped

boolAndGrouped :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolAndGrouped = safeCoerceAgg A.boolAnd

boolAnd :: PGRep a ~ PGBool => Aggregator (Column a) (Column (Nullable a))
boolAnd = toNullable <$> boolAndGrouped

-- TODO Needs opaleye 0.4
--stringAgg :: PGRep a ~ PGText => Column a -> Aggregator (Column a) (Column a)
--stringAgg = safeCoerceAgg . A.stringAgg . safeCoerceToRep

-- TODO: Missing arrayAgg

safeCoerceAgg :: Profunctor p
  => p (Column (PGRep a)) (Column (PGRep b))
  -> p (Column        a ) (Column        b )
safeCoerceAgg = dimap safeCoerceToRep safeCoerceFromRep
