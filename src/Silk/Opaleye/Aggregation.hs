{-# LANGUAGE
    FlexibleContexts
  , NoMonomorphismRestriction
  , TypeFamilies
  #-}
module Silk.Opaleye.Aggregation
  ( sum_
  , count
  , avg
  , max_
  , min_
  , boolOr
  , boolAnd
  , stringAgg
  -- * Re-exports
  , Aggregator
  , aggregate
  , groupBy
  ) where

import Data.Int (Int64)
import Data.Profunctor (Profunctor, dimap)

import Opaleye.Aggregate (Aggregator, aggregate, groupBy)
import Opaleye.Column (Column)
import Opaleye.PGTypes (PGBool, PGText)
import qualified Opaleye.Aggregate as A (avg, boolAnd, boolOr, count, max, min, stringAgg, sum)

import Silk.Opaleye.ShowConstant

sum_ :: Aggregator (Column a) (Column a)
sum_ = safeAgg A.sum

count :: Aggregator (Column a) (Column Int64)
count = dimap id safeCoerceFromRep A.count

avg :: Aggregator (Column Double) (Column Double)
avg = safeAgg A.avg

max_ :: PGOrd (PGRep a) => Aggregator (Column a) (Column a)
max_ = safeAgg A.max

min_ :: PGOrd (PGRep a) => Aggregator (Column a) (Column a)
min_ = safeAgg A.min

boolOr :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolOr = safeAgg A.boolOr

boolAnd :: PGRep a ~ PGBool => Aggregator (Column a) (Column a)
boolAnd = safeAgg A.boolAnd

stringAgg :: PGRep a ~ PGText => Column a -> Aggregator (Column a) (Column a)
stringAgg = safeAgg . A.stringAgg . safeCoerceToRep

-- TODO: Missing arrayAgg

safeAgg :: Profunctor p
  => p (Column (PGRep a)) (Column (PGRep b))
  -> p (Column        a ) (Column        b )
safeAgg = dimap safeCoerceToRep safeCoerceFromRep
