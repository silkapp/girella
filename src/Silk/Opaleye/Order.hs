{-# LANGUAGE FlexibleContexts #-}
module Silk.Opaleye.Order
  ( Order
  , asc
  , desc
  , orderBy
  ) where

import Opaleye.Column (Column)
import Opaleye.Order (Order, orderBy)
import qualified Opaleye.Order as O

import Silk.Opaleye.ShowConstant

asc :: (ShowConstant b, PGOrd (PGRep b)) => (a -> Column b) -> Order a
asc f = O.asc $ safeCoerceToRep . f

desc :: (ShowConstant b, PGOrd (PGRep b)) => (a -> Column b) -> Order a
desc f = O.desc $ safeCoerceToRep . f
