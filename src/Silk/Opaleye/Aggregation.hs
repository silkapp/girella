{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  , TypeFamilies
  #-}
module Silk.Opaleye.Aggregation
  ( sum_
  , sumGrouped
  , sumNullable
  , count
  , avg
  , avgGrouped
  , avgNullable
  , max_
  , maxGrouped
  , maxNullable
  , min_
  , minGrouped
  , minNullable
  , boolOr
  , boolOrGrouped
  , boolOrNullable
  , boolAnd
  , boolAndGrouped
  , boolAndNullable
  , arrayAgg
  , arrayAggGrouped
  , stringAgg
  , stringAggGrouped
  , safeCoerceAgg
  -- * Default aggregators
  , GroupBy (getGroupBy)
  -- * Renames
  , groupBy_
  -- * Re-exports
  , Aggregator
  , aggregate
  , orderAggregate
  ) where

import Data.Int (Int64)
import Data.Profunctor (Profunctor, dimap, lmap, rmap)
import Data.Profunctor.Product (ProductProfunctor, (***!))
import Data.Profunctor.Product.Default (Default (def))
import qualified Data.Profunctor.Product as PP

import Opaleye.Aggregate (Aggregator, aggregate, orderAggregate)
import Opaleye.Column (Column, Nullable, toNullable)
import Opaleye.PGTypes (PGArray, PGBool, PGText)
import qualified Opaleye.Aggregate as A

import Silk.Opaleye.Compat (PGOrd, unsafeCoerceColumn)
import Silk.Opaleye.ShowConstant (PGRep, ShowConstant, safeCoerceFromRep, safeCoerceToRep)

groupBy_ :: Aggregator (Column a) (Column a)
groupBy_ = A.groupBy

-- TODO Constrain to numeric types.
-- http://www.postgresql.org/docs/9.4/static/functions-aggregate.html
sumGrouped :: ShowConstant a => Aggregator (Column a) (Column a)
sumGrouped = safeCoerceAgg A.sum

sumNullable :: ShowConstant a => Aggregator (Column (Nullable a)) (Column (Nullable a))
sumNullable = A.sum

sum_ :: ShowConstant a => Aggregator (Column a) (Column (Nullable a))
sum_ = toNullable <$> sumGrouped

count :: ShowConstant a => Aggregator (Column a) (Column Int64)
count = rmap safeCoerceFromRep A.count

-- TODO Expand to numeric input (even integral)
-- http://www.postgresql.org/docs/9.4/static/functions-aggregate.html
avgGrouped :: Aggregator (Column Double) (Column Double)
avgGrouped = safeCoerceAgg A.avg

avgNullable :: Aggregator (Column (Nullable Double)) (Column (Nullable Double))
avgNullable = dimap unsafeCoerceColumn unsafeCoerceColumn A.avg

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

boolOrNullable :: PGRep a ~ PGBool => Aggregator (Column (Nullable a)) (Column (Nullable a))
boolOrNullable = nullableAgg boolOr

boolAndGrouped :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolAndGrouped = safeCoerceAgg A.boolAnd

boolAnd :: PGRep a ~ PGBool => Aggregator (Column a) (Column (Nullable a))
boolAnd = toNullable <$> boolAndGrouped

boolAndNullable :: PGRep a ~ PGBool => Aggregator (Column (Nullable a)) (Column (Nullable a))
boolAndNullable = nullableAgg boolAnd

stringAggGrouped :: PGRep a ~ PGText => Column a -> Aggregator (Column a) (Column a)
stringAggGrouped = safeCoerceAgg . A.stringAgg . safeCoerceToRep

stringAgg :: PGRep a ~ PGText => Column a -> Aggregator (Column a) (Column (Nullable a))
stringAgg = fmap toNullable . stringAggGrouped

arrayAggGrouped :: (PGRep a ~ rep, PGRep arr ~ PGArray rep) => Aggregator (Column a) (Column arr)
arrayAggGrouped = safeCoerceAgg A.arrayAgg

arrayAgg :: (PGRep a ~ rep, PGRep arr ~ PGArray rep) => Aggregator (Column a) (Column (Nullable arr))
arrayAgg = toNullable <$> arrayAggGrouped

safeCoerceAgg :: Profunctor p
  => p (Column (PGRep a)) (Column (PGRep b))
  -> p (Column        a ) (Column        b )
safeCoerceAgg = dimap safeCoerceToRep safeCoerceFromRep

nullableAgg
  :: Profunctor p
  => p (Column a) (Column b)
  -> p (Column (Nullable a)) (Column b)
nullableAgg = lmap unsafeCoerceColumn

newtype GroupBy a b = GroupBy { getGroupBy :: Aggregator a b }

instance Default GroupBy (Column a) (Column a) where
  def = GroupBy groupBy_

instance Profunctor GroupBy where
  dimap f g (GroupBy agg) = GroupBy (dimap f g agg)

instance ProductProfunctor GroupBy where
  empty = GroupBy PP.empty
  GroupBy agg1 ***! GroupBy agg2 = GroupBy (agg1 ***! agg2)
