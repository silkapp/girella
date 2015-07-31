{-# OPTIONS -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE
    CPP
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , ScopedTypeVariables
  , TypeFamilies
  , UndecidableInstances
  #-}
module Silk.Opaleye.ShowConstant
  ( ShowConstant (..)
  , safeCoerceToRep
  , safeCoerceFromRep
  , safelyWrapped
  ) where

import Data.CaseInsensitive (CI)
import Data.Int (Int64)
import Data.String.Conversions
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import qualified Data.Text      as S
import qualified Data.Text.Lazy as L

import Opaleye.Column (Column)
import Opaleye.PGTypes
import Opaleye.RunQuery (QueryRunnerColumn, queryRunnerColumn)

import Silk.Opaleye.Compat (QueryRunnerColumnDefault (..), unsafeCoerceColumn)

class ShowConstant a where
  type PGRep a :: *
  constant :: a -> Column a

safeCoerceToRep :: PGRep a ~ b => Column a -> Column b
safeCoerceToRep = unsafeCoerceColumn

safeCoerceFromRep :: PGRep a ~ b => Column b -> Column a
safeCoerceFromRep = unsafeCoerceColumn

safelyWrapped :: (Column (PGRep a) -> Column (PGRep b)) -> Column a -> Column b
safelyWrapped f = safeCoerceFromRep . f . safeCoerceToRep

instance ShowConstant [Char] where
  type PGRep String = PGText
  constant = safeCoerceFromRep . pgString
instance QueryRunnerColumnDefault [Char] [Char] where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant StrictText where
  type PGRep S.Text = PGText
  constant = safeCoerceFromRep . pgStrictText
instance QueryRunnerColumnDefault StrictText StrictText where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant LazyText where
  type PGRep L.Text = PGText
  constant = safeCoerceFromRep . pgLazyText
instance QueryRunnerColumnDefault LazyText LazyText where
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
