{-# LANGUAGE FlexibleContexts #-}
module Silk.Opaleye.Order
  ( Order
  , asc
  , desc
  , orderBy
  ) where

import Opaleye.Column (Column)
import Opaleye.Order (Order, orderBy)
import Opaleye.Order (PGOrd)
import qualified Opaleye.Order as O

import Silk.Opaleye.ShowConstant

asc :: PGOrd (PGRep b) => (a -> Column b) -> Order a
asc f = O.asc $ safeCoerceToRep . f

desc :: PGOrd (PGRep b) => (a -> Column b) -> Order a
desc f = O.desc $ safeCoerceToRep . f
