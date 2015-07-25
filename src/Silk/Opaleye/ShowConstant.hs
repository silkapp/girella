{-# OPTIONS -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE
    CPP
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
  , TString (..)
  , TInt4 (..)
  , TInt8 (..)
  , TDouble (..)
  , TUUID (..)
  , TDate (..)
  , TTimestamptz (..)
  , TTimestamp (..)
  , TTime (..)
  , TCIText (..)
  , PGOrd
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
#if MIN_VERSION_opaleye(0,4,0)
import Opaleye.Order (PGOrd)
#endif

import Silk.Opaleye.Compat (QueryRunnerColumnDefault (..), unsafeCoerceColumn)

import Opaleye.Column ()

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
  constant = constantString
instance QueryRunnerColumnDefault [Char] [Char] where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant StrictText where
  type PGRep S.Text = PGText
  constant = constantString
instance QueryRunnerColumnDefault S.Text S.Text where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant LazyText where
  type PGRep L.Text = PGText
  constant = constantString
instance QueryRunnerColumnDefault L.Text L.Text where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Int where
  type PGRep Int = PGInt4
  constant = constantInt4
instance QueryRunnerColumnDefault Int Int where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Int64 where
  type PGRep Int64 = PGInt8
  constant = constantInt8
instance QueryRunnerColumnDefault Int64 Int64 where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Double where
  type PGRep Double = PGFloat8
  constant = constantDouble
instance QueryRunnerColumnDefault Double Double where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Bool where
  type PGRep Bool = PGBool ; constant = constantBool
instance QueryRunnerColumnDefault Bool Bool where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant UUID where
  type PGRep UUID = PGUuid
  constant = constantUUID
instance QueryRunnerColumnDefault UUID UUID where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant Day where
  type PGRep Day = PGDate
  constant = constantDay
instance QueryRunnerColumnDefault Day Day where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant UTCTime where
  type PGRep UTCTime = PGTimestamptz
  constant = constantTimestamptz
instance QueryRunnerColumnDefault UTCTime UTCTime where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant LocalTime where
  type PGRep LocalTime = PGTimestamp
  constant = constantTimestamp
instance QueryRunnerColumnDefault LocalTime LocalTime where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant TimeOfDay where
  type PGRep TimeOfDay = PGTime
  constant = constantTime
instance QueryRunnerColumnDefault TimeOfDay TimeOfDay where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant (CI S.Text) where
  type PGRep (CI S.Text) = PGCitext
  constant = constantCIText
instance QueryRunnerColumnDefault (CI S.Text) (CI S.Text) where
  queryRunnerColumnDefault = qrcDef

instance ShowConstant (CI L.Text) where
  type PGRep (CI L.Text) = PGCitext
  constant = constantCIText
instance QueryRunnerColumnDefault (CI L.Text) (CI L.Text) where
  queryRunnerColumnDefault = qrcDef

qrcDef :: forall a b c . (PGRep a ~ b, QueryRunnerColumnDefault b c) => QueryRunnerColumn a c
qrcDef = queryRunnerColumn (safeCoerceToRep :: Column a -> Column b) id queryRunnerColumnDefault

class TString a where
  constantString :: a -> Column a

instance TString [Char] where
  constantString = safeCoerceFromRep . pgString

instance TString StrictText where
  constantString = safeCoerceFromRep . pgStrictText

instance TString LazyText where
  constantString = safeCoerceFromRep . pgLazyText

class TInt4 a where
  constantInt4 :: a -> Column a

class TInt8 a where
  constantInt8 :: a -> Column a

instance TInt4 Int where
  constantInt4 = safeCoerceFromRep . pgInt4

instance TInt8 Int64 where
  constantInt8 = safeCoerceFromRep . pgInt8

class TDouble a where
  constantDouble :: a -> Column a

instance TDouble Double where
  constantDouble = safeCoerceFromRep . pgDouble

class TBool a where
  constantBool :: a -> Column a

instance TBool Bool where
  constantBool = safeCoerceFromRep . pgBool

class TUUID a where
  constantUUID :: a -> Column a

instance TUUID UUID where
  constantUUID = safeCoerceFromRep . pgUUID

class TDate a where
  constantDay :: a -> Column a

instance TDate Day where
  constantDay = safeCoerceFromRep . pgDay

class TTimestamptz a where
  constantTimestamptz :: a -> Column a

instance TTimestamptz UTCTime where
  constantTimestamptz = safeCoerceFromRep . pgUTCTime

class TTimestamp a where
  constantTimestamp :: a -> Column a

instance TTimestamp LocalTime where
  constantTimestamp = safeCoerceFromRep . pgLocalTime

class TTime a where
  constantTime :: a -> Column a

instance TTime TimeOfDay where
  constantTime = safeCoerceFromRep . pgTimeOfDay

class TCIText a where
  constantCIText :: a -> Column a

instance TCIText (CI StrictText) where
  constantCIText = safeCoerceFromRep . pgCiStrictText

instance TCIText (CI LazyText) where
  constantCIText = safeCoerceFromRep . pgCiLazyText

#if !MIN_VERSION_opaleye(0,4,0)
class PGOrd a
#endif
instance PGOrd Int
instance PGOrd Int64
instance PGOrd UTCTime
