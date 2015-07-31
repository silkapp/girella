{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , TypeFamilies
  #-}
module Silk.Opaleye.Operators
  ( (.==)
  , (.==?)
  , (./=)
  , (.||)
  , (.&&)
  , (.>)
  , (.<)
  , (.>=)
  , (.<=)
  , lower
  , ors
  , ands
  , in_ -- TODO rename
  , notIn
  , isNull
  , not_
  , null_
  , nullable
  , maybeToNullable
  , fromNullable
  , unsafeCast
  ) where

import Prelude hiding (foldr, (.))

import Control.Category ((.))
import Data.Foldable

import Opaleye.Column (toNullable, unsafeCast)
import Opaleye.PGTypes (PGText)
import qualified Opaleye.Column    as C
import qualified Opaleye.Operators as O

import Silk.Opaleye.ShowConstant
import Silk.Opaleye.TH

infix 4 .==
-- | Equality between columns, does not allow comparison on Nullable
-- to avoid confusion since SQL and haskell semantics differ there.
(.==) :: ShowConstant a => Column a -> Column a -> Column Bool
a .== b = safeCoerceFromRep $ a O..== b

infix 4 .==?
-- | Compare two nullables with SQL semantics, null /= null
(.==?) :: ShowConstant a => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .==? b = safeCoerceFromRep $ a O..== b

infix 4 ./=
(./=) :: ShowConstant a => Column a -> Column a -> Column Bool
a ./= b = safeCoerceFromRep $ a O../= b

infixr 2 .||
(.||) :: Column Bool -> Column Bool -> Column Bool
a .|| b = safeCoerceFromRep $ safeCoerceToRep a O..|| safeCoerceToRep b

infixr 3 .&&
(.&&) :: Column Bool -> Column Bool -> Column Bool
a .&& b = safeCoerceFromRep $ safeCoerceToRep a O..&& safeCoerceToRep b

infix 4 .>
(.>) :: (ShowConstant a, PGOrd (PGRep a)) => Column a -> Column a -> Column Bool
a .> b = safeCoerceFromRep $ safeCoerceToRep a O..> safeCoerceFromRep b

infix 4 .<
(.<) :: (ShowConstant a, PGOrd (PGRep a)) => Column a -> Column a -> Column Bool
a .< b = safeCoerceFromRep $ safeCoerceToRep a O..< safeCoerceToRep b

infix 4 .>=
(.>=) :: (ShowConstant a, PGOrd (PGRep a)) => Column a -> Column a -> Column Bool
a .>= b = safeCoerceFromRep $ safeCoerceToRep a O..>= safeCoerceToRep b

infix 4 .<=
(.<=) :: (ShowConstant a, PGOrd (PGRep a)) => Column a -> Column a -> Column Bool
a .<= b = safeCoerceFromRep $ safeCoerceToRep a O..<= safeCoerceToRep b

lower :: PGRep a ~ PGText => Column a -> Column a
lower = safeCoerceFromRep . O.lower . safeCoerceToRep

-- Query helpers

-- | 'Data.list.any' for 'Column'
ors :: Foldable f => f (Column Bool) -> Column Bool
ors = foldr (.||) (constant False)

-- | 'Data.List.all' for 'Column'
ands :: Foldable f => f (Column Bool) -> Column Bool
ands = foldr (.&&) (constant True)

-- | 'Data.List.elem' for Column.
in_ :: ShowConstant o => [Column o] -> Column o -> Column Bool
in_ hs w = ors . map (w .==) $ hs

-- | 'Data.List.notElem' for 'Column'.
notIn :: ShowConstant a => [Column a] -> Column a -> Column Bool
notIn hs w = ands . map (./= w) $ hs

-- | 'isJust' for 'Column'.
isNull :: Column (Nullable a) -> Column Bool
isNull = safeCoerceFromRep . C.isNull

-- Avoiding clashes with prelude

-- | Boolean negation.
not_ :: Column Bool -> Column Bool
not_ = safeCoerceFromRep . O.not . safeCoerceToRep

-- | 'Nothing' for 'Column's.
null_ :: Column (Nullable a)
null_ = C.null

-- Implicit calls to ShowConstant

-- | Turn a value into a 'Nullable' 'Column', it's essentially 'Just'.
nullable :: ShowConstant a => a -> Column (Nullable a)
nullable = toNullable . constant

-- | Convert a 'Maybe' into a 'Column'.
maybeToNullable :: ShowConstant a => Maybe a -> Column (Nullable a)
maybeToNullable = maybe null_ nullable

-- | 'fromMaybe' for 'Column'.
fromNullable :: ShowConstant a => a -> Column (Nullable a) -> Column a
fromNullable = C.fromNullable . constant
