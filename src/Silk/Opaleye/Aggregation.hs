module Silk.Opaleye.Aggregation
  ( Aggregator
  , aggregate
  , boolAnd
  , boolOr
  , count
  , groupBy
  , maxColumn
  , minColumn
  ) where

import Data.Profunctor (dimap)

import Opaleye.Aggregate (Aggregator, aggregate, count, groupBy)
import Opaleye.Column (Column)
import qualified Opaleye.Aggregate as A (boolAnd, boolOr, max, min)

import Silk.Opaleye.ShowConstant (fromPGBool, toPGBool)

boolOr :: Aggregator (Column Bool) (Column Bool)
boolOr = dimap toPGBool fromPGBool A.boolOr

boolAnd :: Aggregator (Column Bool) (Column Bool)
boolAnd = dimap toPGBool fromPGBool A.boolAnd

maxColumn :: Aggregator (Column a) (Column a)
maxColumn = A.max

minColumn :: Aggregator (Column a) (Column a)
minColumn = A.min
