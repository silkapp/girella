-- | Compatibility for code written for older versions of Opaleye.
-- This module will disappear at some point.
module Silk.Opaleye.Compat where

import Control.Arrow
import Opaleye.Column
import Opaleye.Operators
import Opaleye.PGTypes
import Opaleye.QueryArr

type Wire = Column

type ExprArr = (->)

type Expr = (->) ()

toQueryArrDef :: (a -> b) -> QueryArr a b
toQueryArrDef = arr

-- Arrowized operators, these will probably be removed since the new operators are nicer to work with

infix 4 .==.
(.==.) :: (a -> Column b) -> (a -> Column b) -> (a -> Column PGBool)
(.==.) x y a = x a .== y a

infixr 3 .&&.
(.&&.) :: ExprArr a (Column PGBool) -> ExprArr a (Column PGBool) -> ExprArr a (Column PGBool)
f .&&. g = uncurry (.&&) <<< (f &&& g)

infixr 2 .||.
(.||.) :: ExprArr a (Column PGBool) -> ExprArr a (Column PGBool) -> ExprArr a (Column PGBool)
f .||. g = uncurry (.||) <<< (f &&& g)
