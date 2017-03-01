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
  , quot_
  , rem_
  , trunc
  , upper
  , lower
  , like
  , charLength
  , ors
  , ands
  , in_
  , case_
  , ifThenElse
  , notIn
  , isNull
  , not_
  , null_
  , nullable
  , maybeToNullable
  , C.fromNullable
  , unsafeCast
  ) where

import Prelude.Compat hiding ((.))

import Control.Arrow ((***))
import Control.Category ((.))

import Opaleye.Column (Nullable, toNullable, unsafeCast)
import Opaleye.Internal.Column (Column (Column), PGFractional)
import Opaleye.Internal.HaskellDB.PrimQuery (PrimExpr (FunExpr))
import Opaleye.PGTypes (PGBool, PGText, pgBool)
import qualified Opaleye.Column    as C (fromNullable, isNull, null)
import qualified Opaleye.Operators as O

import Silk.Opaleye.Compat (PGIntegral, PGOrd, PGString)
import Silk.Opaleye.ShowConstant (ShowConstant (constant), PGRep, safeCoerceFromRep, safeCoerceToRep)

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
a .>? b = safeCoerceFromRep $ safeCoerceToRep a O..> safeCoerceToRep b

infix 4 .<
(.<) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .< b = safeCoerceFromRep $ safeCoerceToRep a O..< safeCoerceToRep b

infix 4 .<?
(.<?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .<? b = safeCoerceFromRep $ safeCoerceToRep a O..< safeCoerceToRep b

infix 4 .>=
(.>=) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .>= b = safeCoerceFromRep $ safeCoerceToRep a O..>= safeCoerceToRep b

infix 4 .>=?
(.>=?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .>=? b = safeCoerceFromRep $ safeCoerceToRep a O..>= safeCoerceToRep b

infix 4 .<=
(.<=) :: PGOrd (PGRep a) => Column a -> Column a -> Column Bool
a .<= b = safeCoerceFromRep $ safeCoerceToRep a O..<= safeCoerceToRep b

infix 4 .<=?
(.<=?) :: PGOrd (PGRep a) => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .<=? b = safeCoerceFromRep $ safeCoerceToRep a O..<= safeCoerceToRep b

quot_ :: PGIntegral (PGRep a) => Column a -> Column a -> Column a
quot_ a b = safeCoerceFromRep $ safeCoerceToRep a `O.quot_` safeCoerceToRep b

rem_ :: PGIntegral (PGRep a) => Column a -> Column a -> Column a
rem_ a b = safeCoerceFromRep $ safeCoerceToRep a `O.rem_` safeCoerceToRep b

-- These

upper :: PGRep a ~ PGText => Column a -> Column a
upper = safeCoerceFromRep . O.upper . safeCoerceToRep

lower :: PGRep a ~ PGText => Column a -> Column a
lower = safeCoerceFromRep . O.lower . safeCoerceToRep

like :: PGRep a ~ PGText => Column a -> Column a -> Column Bool
like a = safeCoerceFromRep . O.like (safeCoerceToRep a) . safeCoerceToRep

charLength :: PGString (PGRep a) => Column a -> Column Int
charLength (Column e) = Column (FunExpr "char_length" [e])

trunc :: PGFractional (PGRep a) => Column a -> Column Int
trunc (Column e) = Column (FunExpr "trunc" [e])

case_ :: ShowConstant a => [(Column Bool, Column a)] -> Column a -> Column a
case_ xs = safeCoerceFromRep . O.case_ (map (safeCoerceToRep *** safeCoerceToRep) xs) . safeCoerceToRep

ifThenElse :: Column Bool -> Column a -> Column a -> Column a
ifThenElse = O.ifThenElse . safeCoerceToRep

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
-- TODO: this doesn't match the opaleye semantics, which is confusing.
-- But we already use it in many places.
maybeToNullable :: ShowConstant a => Maybe a -> Column (Nullable a)
maybeToNullable = maybe null_ nullable
