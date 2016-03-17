{-# OPTIONS -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  , FlexibleContexts
  #-}
module Silk.Opaleye.ShowConstant
  ( ShowConstant (..)
  , safeCoerceToRep
  , safeCoerceFromRep
  , safelyWrapped
  ) where

import Data.CaseInsensitive (CI)
import Data.Typeable (Typeable)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.String.Conversions
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)

import Opaleye.Internal.Column (Column (Column))
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (OtherLit), PrimExpr (ConstExpr))
import Opaleye.Internal.HaskellDB.Sql.Default (defaultSqlGenerator, defaultSqlLiteral)
import Opaleye.PGTypes
import Opaleye.RunQuery (QueryRunnerColumn, queryRunnerColumn)

import Silk.Opaleye.Compat (QueryRunnerColumnDefault (..), unsafeCoerceColumn)

-- | A type that can be serialized and used in the database. 'PGRep'
-- gives the opaleye type and 'constant' converts a value to a
-- 'Column'.
--
-- This type can not be nullable since that leads to unintuitive
--behavior when using operands that have special cases for null.
--
-- Note that 'constant' gives us 'Column a' and not 'Column (PGRep
-- a)'. The reason is that we don't want two types (e.g. two primary
-- keys) to be comparable just because the have the same underlying
-- type.
--
-- Girella's API doesn't use the internal 'PGTypes', everything is
-- mapped to Haskell types.
--
-- The instances defined in this module correspond directly to the
-- types Opaleye supports.
class ShowConstant a where
  type PGRep a :: *
  constant :: a -> Column a

-- | It's always safe to coerce to the underlying representation.
safeCoerceToRep :: PGRep a ~ b => Column a -> Column b
safeCoerceToRep = unsafeCoerceColumn

-- | It's always safe to coerce from  the underlying representation.
safeCoerceFromRep :: PGRep a ~ b => Column b -> Column a
safeCoerceFromRep = unsafeCoerceColumn

-- Perform a db operation on the underlying type.
safelyWrapped :: (Column (PGRep a) -> Column (PGRep b)) -> Column a -> Column b
safelyWrapped f = safeCoerceFromRep . f . safeCoerceToRep

instance ShowConstant a => ShowConstant [a] where
  type PGRep [a] = PGArray (PGRep a)
  constant = safeCoerceFromRep . pgArray (safeCoerceToRep . constant)

pgArray :: (a -> Column b) -> [a] -> Column (PGArray b)
pgArray pgEl xs = literalColumn (OtherLit $ "ARRAY[" ++ intercalate "," (map oneEl xs) ++ "]")
  where
    oneEl x = case pgEl x of
      (Column (ConstExpr lit)) -> defaultSqlLiteral defaultSqlGenerator lit
      (Column _) -> error "pgArray: element function should always produce a constant."

instance (Typeable b, QueryRunnerColumnDefault a b)
  => QueryRunnerColumnDefault [a] [b] where
  queryRunnerColumnDefault = queryRunnerColumn unsafeCoerceColumn id
                               (queryRunnerColumnDefault :: QueryRunnerColumn (PGArray a) [b])

instance ShowConstant StrictText where
  type PGRep StrictText = PGText
  constant = safeCoerceFromRep . pgStrictText
instance QueryRunnerColumnDefault StrictText StrictText where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant LazyText where
  type PGRep LazyText = PGText
  constant = safeCoerceFromRep . pgLazyText
instance QueryRunnerColumnDefault LazyText LazyText where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant LazyByteString where
  type PGRep LazyByteString = PGBytea
  constant = safeCoerceFromRep . pgLazyByteString
instance QueryRunnerColumnDefault LazyByteString LazyByteString where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant StrictByteString where
  type PGRep StrictByteString = PGBytea
  constant = safeCoerceFromRep . pgStrictByteString
instance QueryRunnerColumnDefault StrictByteString StrictByteString where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Int where
  type PGRep Int = PGInt4
  constant = safeCoerceFromRep . pgInt4
instance QueryRunnerColumnDefault Int Int where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Int64 where
  type PGRep Int64 = PGInt8
  constant = safeCoerceFromRep . pgInt8
instance QueryRunnerColumnDefault Int64 Int64 where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Double where
  type PGRep Double = PGFloat8
  constant = safeCoerceFromRep . pgDouble
instance QueryRunnerColumnDefault Double Double where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Bool where
  type PGRep Bool = PGBool
  constant = safeCoerceFromRep . pgBool
instance QueryRunnerColumnDefault Bool Bool where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant UUID where
  type PGRep UUID = PGUuid
  constant = safeCoerceFromRep . pgUUID
instance QueryRunnerColumnDefault UUID UUID where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Day where
  type PGRep Day = PGDate
  constant = safeCoerceFromRep . pgDay
instance QueryRunnerColumnDefault Day Day where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant UTCTime where
  type PGRep UTCTime = PGTimestamptz
  constant = safeCoerceFromRep . pgUTCTime
instance QueryRunnerColumnDefault UTCTime UTCTime where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant LocalTime where
  type PGRep LocalTime = PGTimestamp
  constant = safeCoerceFromRep . pgLocalTime
instance QueryRunnerColumnDefault LocalTime LocalTime where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant TimeOfDay where
  type PGRep TimeOfDay = PGTime
  constant = safeCoerceFromRep . pgTimeOfDay
instance QueryRunnerColumnDefault TimeOfDay TimeOfDay where
  queryRunnerColumnDefault = qrcDef

-- postgresql-simple only defines 'CI' instances for strict & lazy
-- 'Text' so we can't do more either.

instance ShowConstant (CI StrictText) where
  type PGRep (CI StrictText) = PGCitext
  constant = safeCoerceFromRep . pgCiStrictText
instance QueryRunnerColumnDefault (CI StrictText) (CI StrictText) where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant (CI LazyText) where
  type PGRep (CI LazyText) = PGCitext
  constant = safeCoerceFromRep . pgCiLazyText
instance QueryRunnerColumnDefault (CI LazyText) (CI LazyText) where
  queryRunnerColumnDefault = qrcDef

qrcDef :: forall a b c . (PGRep a ~ b, QueryRunnerColumnDefault b c) => QueryRunnerColumn a c
qrcDef = queryRunnerColumn (safeCoerceToRep :: Column a -> Column b) id queryRunnerColumnDefault
