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

asc :: (TOrd b, PGOrd (OrdRep b)) => (a -> Column b) -> Order a
asc f = O.asc $ ordCoerce . f

desc :: (TOrd b, PGOrd (OrdRep b)) => (a -> Column b) -> Order a
desc f = O.desc $ ordCoerce . f
