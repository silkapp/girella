{-# OPTIONS -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  , UndecidableInstances
  #-}
module Silk.Opaleye.ShowConstant
  ( PGRep
  , ShowConstant (..)
  , safeCoerceToRep
  , safeCoerceFromRep
  , safelyWrapped
  , safeCoerce
  , emptyArray
  , singletonArray
  , arrayPrepend
  , IsPGType (showPGType)
  , PGTextual
  , PGIntegral
  ) where

import Data.CaseInsensitive (CI)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.String.Conversions
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Typeable (Typeable)
import Data.UUID (UUID)

import Opaleye.Column (unsafeCast)
import Opaleye.Internal.Column (Column (Column), Nullable, PGNum (..), PGFractional (..))
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (OtherLit), PrimExpr (ConstExpr, FunExpr))
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

-- | The postgres representation of a Haskell type.

type family PGRep a :: *

-- | It's always safe to coerce to the underlying representation.
safeCoerceToRep :: PGRep a ~ b => Column a -> Column b
safeCoerceToRep = unsafeCoerceColumn

-- | It's always safe to coerce from  the underlying representation.
safeCoerceFromRep :: PGRep a ~ b => Column b -> Column a
safeCoerceFromRep = unsafeCoerceColumn

-- Perform a db operation on the underlying type.
safelyWrapped :: (Column (PGRep a) -> Column (PGRep b)) -> Column a -> Column b
safelyWrapped f = safeCoerceFromRep . f . safeCoerceToRep

safeCoerce :: PGRep a ~ PGRep b => Column a -> Column b
safeCoerce = safelyWrapped id

-- | A class for Haskell values that can be converted to postgres
-- literal constants.

class ShowConstant a where
  constant :: a -> Column a

type instance PGRep (Maybe a) = Nullable (PGRep a)
type instance PGRep (Nullable a) = Nullable (PGRep a)

type instance PGRep [a] = PGArray (PGRep a)
instance (ShowConstant a, IsPGType (PGRep a)) => ShowConstant [a] where
  constant = safeCoerceFromRep . pgArray (safeCoerceToRep . constant)

emptyArray :: IsPGType (PGRep a) => Column [a]
emptyArray = safeCoerceFromRep $ pgArray id []

arrayPrepend :: Column a -> Column [a] -> Column [a]
arrayPrepend (Column e) (Column es) = Column (FunExpr "array_prepend" [e, es])

singletonArray :: forall a. IsPGType (PGRep a) => Column a -> Column [a]
singletonArray x = arrayPrepend x emptyArray

pgArray :: forall a b. IsPGType b => (a -> Column b) -> [a] -> Column (PGArray b)
pgArray pgEl xs = unsafeCast arrayTy $
  literalColumn (OtherLit $ "ARRAY[" ++ intercalate "," (map oneEl xs) ++ "]")
  where
    oneEl x = case pgEl x of
      (Column (ConstExpr lit)) -> defaultSqlLiteral defaultSqlGenerator lit
      (Column _) -> error "pgArray: element function should always produce a constant."
    arrayTy = showPGType ([] :: [PGArray b])

instance (Typeable b, QueryRunnerColumnDefault a b)
  => QueryRunnerColumnDefault [a] [b] where
  queryRunnerColumnDefault = queryRunnerColumn unsafeCoerceColumn id
                               (queryRunnerColumnDefault :: QueryRunnerColumn (PGArray a) [b])

type instance PGRep StrictText = PGText
instance ShowConstant StrictText where
  constant = safeCoerceFromRep . pgStrictText
instance QueryRunnerColumnDefault StrictText StrictText where
  queryRunnerColumnDefault = qrcDef

type instance PGRep LazyText = PGText
instance ShowConstant LazyText where
  constant = safeCoerceFromRep . pgLazyText
instance QueryRunnerColumnDefault LazyText LazyText where
  queryRunnerColumnDefault = qrcDef

type instance PGRep LazyByteString = PGBytea
instance ShowConstant LazyByteString where
  constant = safeCoerceFromRep . pgLazyByteString
instance QueryRunnerColumnDefault LazyByteString LazyByteString where
  queryRunnerColumnDefault = qrcDef

type instance PGRep StrictByteString = PGBytea
instance ShowConstant StrictByteString where
  constant = safeCoerceFromRep . pgStrictByteString
instance QueryRunnerColumnDefault StrictByteString StrictByteString where
  queryRunnerColumnDefault = qrcDef

type instance PGRep Int = PGInt4
instance PGNum Int where
  pgFromInteger = safeCoerceFromRep . pgFromInteger
instance ShowConstant Int where
  constant = safeCoerceFromRep . pgInt4
instance QueryRunnerColumnDefault Int Int where
  queryRunnerColumnDefault = qrcDef

type instance PGRep Int64 = PGInt8
instance PGNum Int64 where
  pgFromInteger = safeCoerceFromRep . pgFromInteger
instance ShowConstant Int64 where
  constant = safeCoerceFromRep . pgInt8
instance QueryRunnerColumnDefault Int64 Int64 where
  queryRunnerColumnDefault = qrcDef

type instance PGRep Double = PGFloat8
instance PGNum Double where
  pgFromInteger = safeCoerceFromRep . pgFromInteger
instance PGFractional Double where
  pgFromRational = safeCoerceFromRep . pgFromRational
instance ShowConstant Double where
  constant = safeCoerceFromRep . pgDouble
instance QueryRunnerColumnDefault Double Double where
  queryRunnerColumnDefault = qrcDef

type instance PGRep Bool = PGBool
instance ShowConstant Bool where
  constant = safeCoerceFromRep . pgBool
instance QueryRunnerColumnDefault Bool Bool where
  queryRunnerColumnDefault = qrcDef

type instance PGRep UUID = PGUuid
instance ShowConstant UUID where
  constant = safeCoerceFromRep . pgUUID
instance QueryRunnerColumnDefault UUID UUID where
  queryRunnerColumnDefault = qrcDef

type instance PGRep Day = PGDate
instance ShowConstant Day where
  constant = safeCoerceFromRep . pgDay
instance QueryRunnerColumnDefault Day Day where
  queryRunnerColumnDefault = qrcDef

type instance PGRep UTCTime = PGTimestamptz
instance ShowConstant UTCTime where
  constant = safeCoerceFromRep . pgUTCTime
instance QueryRunnerColumnDefault UTCTime UTCTime where
  queryRunnerColumnDefault = qrcDef

type instance PGRep LocalTime = PGTimestamp
instance ShowConstant LocalTime where
  constant = safeCoerceFromRep . pgLocalTime
instance QueryRunnerColumnDefault LocalTime LocalTime where
  queryRunnerColumnDefault = qrcDef

type instance PGRep TimeOfDay = PGTime
instance ShowConstant TimeOfDay where
  constant = safeCoerceFromRep . pgTimeOfDay
instance QueryRunnerColumnDefault TimeOfDay TimeOfDay where
  queryRunnerColumnDefault = qrcDef

-- postgresql-simple only defines 'CI' instances for strict & lazy
-- 'Text' so we can't do more either.

type instance PGRep (CI StrictText) = PGCitext
instance ShowConstant (CI StrictText) where
  constant = safeCoerceFromRep . pgCiStrictText
instance QueryRunnerColumnDefault (CI StrictText) (CI StrictText) where
  queryRunnerColumnDefault = qrcDef

type instance PGRep (CI LazyText) = PGCitext
instance ShowConstant (CI LazyText) where
  constant = safeCoerceFromRep . pgCiLazyText
instance QueryRunnerColumnDefault (CI LazyText) (CI LazyText) where
  queryRunnerColumnDefault = qrcDef

qrcDef :: forall a b c . (PGRep a ~ b, QueryRunnerColumnDefault b c) => QueryRunnerColumn a c
qrcDef = queryRunnerColumn (safeCoerceToRep :: Column a -> Column b) id queryRunnerColumnDefault

class IsPGType pgType where
  showPGType :: proxy pgType -> String
instance IsPGType PGBool where
  showPGType _ = "boolean"
instance IsPGType PGDate where
  showPGType _ = "date"
instance IsPGType PGFloat4 where
  showPGType _ = "real"
instance IsPGType PGFloat8 where
  showPGType _ = "double precision"
instance IsPGType PGInt8 where
  showPGType _ = "bigint"
instance IsPGType PGInt4 where
  showPGType _ = "integer"
instance IsPGType PGInt2 where
  showPGType _ = "smallint"
instance IsPGType PGNumeric where
  showPGType _ = "numeric"
instance IsPGType PGText where
  showPGType _ = "text"
instance IsPGType PGTime where
  showPGType _ = "time"
instance IsPGType PGTimestamp where
  showPGType _ = "timestamp"
instance IsPGType PGTimestamptz where
  showPGType _ = "timestamp with time zone"
instance IsPGType PGUuid where
  showPGType _ = "uuid"
instance IsPGType PGCitext where
  showPGType _ =  "citext"
instance IsPGType PGBytea where
  showPGType _ = "bytea"
instance IsPGType a => IsPGType (PGArray a) where
  showPGType _ = showPGType ([] :: [a]) ++ "[]"
instance IsPGType a => IsPGType (Nullable a) where
  showPGType _ = showPGType ([] :: [a])

class PGTextual a
instance PGTextual PGText
instance PGTextual PGCitext

class PGIntegral a
instance PGIntegral PGInt2
instance PGIntegral PGInt4
instance PGIntegral PGInt8
