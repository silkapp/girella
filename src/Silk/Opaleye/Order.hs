{-# LANGUAGE FlexibleContexts #-}
module Silk.Opaleye.Order
  ( Order
  , asc
  , ascNullsFirst
  , desc
  , descNullsLast
  , orderBy
  ) where

import Opaleye.Column (Column)
import Opaleye.Order (Order, orderBy)
import qualified Opaleye.Order as O

import Silk.Opaleye.Compat (PGOrd)
import Silk.Opaleye.ShowConstant

asc :: PGOrd (PGRep b) => (a -> Column b) -> Order a
asc f = O.asc $ safeCoerceToRep . f

desc :: PGOrd (PGRep b) => (a -> Column b) -> Order a
desc f = O.desc $ safeCoerceToRep . f

ascNullsFirst :: PGOrd (PGRep b) => (a -> Column b) -> Order a
ascNullsFirst f = O.ascNullsFirst $ safeCoerceToRep . f

descNullsLast :: PGOrd (PGRep b) => (a -> Column b) -> Order a
descNullsLast f = O.descNullsLast $ safeCoerceToRep . f
