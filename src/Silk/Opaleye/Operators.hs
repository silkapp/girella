{-# LANGUAGE
    FlexibleContexts
  , NoImplicitPrelude
  , TypeFamilies
  #-}
module Silk.Opaleye.Operators
  ( (.==)
  , (.==?)
  , (./=)
  , (.||)
  , (.&&)
  , (.>)
  , (.>?)
  , (.<)
  , (.<?)
  , (.>=)
  , (.>=?)
  , (.<=)
  , (.<=?)
  , upper
  , lower
  , like
  , ors
  , ands
  , in_
  , notIn
  , isNull
  , not_
  , null_
  , nullable
  , maybeToNullable
  , fromNullable
  , unsafeCast
  ) where

import Prelude.Compat hiding ((.))

import Control.Category ((.))

import Opaleye.Column (toNullable, unsafeCast)
import Opaleye.PGTypes (PGBool, PGText, pgBool)
import qualified Opaleye.Column    as C
import qualified Opaleye.Operators as O

import Silk.Opaleye.Compat (PGOrd)
import Silk.Opaleye.ShowConstant
import Silk.Opaleye.TH

infix 4 .==
-- | Equality between columns, does not allow comparison on Nullable
-- to avoid confusion since SQL and haskell semantics differ there.
(.==) :: ShowConstant a => Column a -> Column a -> Column Bool
a .== b = safeCoerceFromRep $ a O..== b

infix 4 .==?
-- | Compare two nullables with SQL semantics; null /= null.
-- If you want to check if a value is null, use 'isNull' instead.
(.==?) :: ShowConstant a => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .==? b = safeCoerceFromRep $ a O..== b

infix 4 ./=
(./=) :: ShowConstant a => Column a -> Column a -> Column Bool
a ./= b = safeCoerceFromRep $ a O../= b

infixr 2 .||
(.||) :: PGRep a ~ PGBool => Column a -> Column a -> Column a
a .|| b = safeCoerceFromRep $ safeCoerceToRep a O..|| safeCoerceToRep b

infixr 3 .&&
(.&&) :: PGRep a ~ PGBool => Column a -> Column a -> Column a
a .&& b = safeCoerceFromRep $ safeCoerceToRep a O..&& safeCoerceToRep b

infix 4 .>
(.>) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .> b = safeCoerceFromRep $ safeCoerceToRep a O..> safeCoerceToRep b

infix 4 .>?
(.>?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .>? b = safeCoerceFromRep $ a O..> b

infix 4 .<
(.<) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .< b = safeCoerceFromRep $ safeCoerceToRep a O..< safeCoerceToRep b

infix 4 .<?
(.<?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .<? b = safeCoerceFromRep $ a O..< b

infix 4 .>=
(.>=) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .>= b = safeCoerceFromRep $ safeCoerceToRep a O..>= safeCoerceToRep b

infix 4 .>=?
(.>=?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .>=? b = safeCoerceFromRep $ a O..>= b

infix 4 .<=
(.<=) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .<= b = safeCoerceFromRep $ safeCoerceToRep a O..<= safeCoerceToRep b

infix 4 .<=?
(.<=?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .<=? b = safeCoerceFromRep $ a O..<= b

upper :: PGRep a ~ PGText => Column a -> Column a
upper = safeCoerceFromRep . O.upper . safeCoerceToRep

lower :: PGRep a ~ PGText => Column a -> Column a
lower = safeCoerceFromRep . O.lower . safeCoerceToRep

like :: PGRep a ~ PGText => Column a -> Column a -> Column Bool
like a = safeCoerceFromRep . O.like (safeCoerceToRep a) . safeCoerceToRep

-- Query helpers

-- | 'Data.list.any' for 'Column'
ors :: (PGRep a ~ PGBool, Foldable f) => f (Column a) -> Column Bool
ors = safeCoerceFromRep . foldr (\a b -> safeCoerceToRep a O..|| b) (pgBool False)

-- | 'Data.List.all' for 'Column'
ands :: (PGRep a ~ PGBool, Foldable f) => f (Column a) -> Column Bool
ands = safeCoerceFromRep . foldr (\a b -> safeCoerceToRep a O..&& b) (pgBool True)

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
not_ :: PGRep a ~ PGBool => Column a -> Column a
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
