{-# LANGUAGE
    FlexibleInstances
  , NoMonomorphismRestriction
  , TypeFamilies
  , UndecidableInstances
  #-}
module Silk.Opaleye.ShowConstant (ShowConstant (..), showConstant) where

import Data.CaseInsensitive (CI)
import Data.Int (Int64)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text            as S
import qualified Data.Text.Lazy       as L

import Opaleye.Column (Column, Nullable)
import Opaleye.PGTypes
import qualified Opaleye.Column as C

class ShowConstant h where
  type PGRep h :: *
  constant :: h -> Column (PGRep h)

instance ShowConstant [Char]      where type PGRep String      = PGText        ; constant = pgString
instance ShowConstant S.Text      where type PGRep S.Text      = PGText        ; constant = pgStrictText
instance ShowConstant L.Text      where type PGRep L.Text      = PGText        ; constant = pgLazyText
instance ShowConstant Int         where type PGRep Int         = PGInt4        ; constant = pgInt4
instance ShowConstant Int64       where type PGRep Int64       = PGInt8        ; constant = pgInt8
instance ShowConstant Double      where type PGRep Double      = PGFloat8      ; constant = pgDouble
instance ShowConstant Bool        where type PGRep Bool        = PGBool        ; constant = pgBool
instance ShowConstant UUID        where type PGRep UUID        = PGUuid        ; constant = pgUUID
instance ShowConstant Day         where type PGRep Day         = PGDate        ; constant = pgDay
instance ShowConstant UTCTime     where type PGRep UTCTime     = PGTimestamptz ; constant = pgUTCTime
instance ShowConstant LocalTime   where type PGRep LocalTime   = PGTimestamp   ; constant = pgLocalTime
instance ShowConstant TimeOfDay   where type PGRep TimeOfDay   = PGTime        ; constant = pgTimeOfDay
instance ShowConstant (CI S.Text) where type PGRep (CI S.Text) = PGCiText      ; constant = pgCiStrictText
instance ShowConstant (CI L.Text) where type PGRep (CI L.Text) = PGCiText      ; constant = pgCiLazyText


showConstant :: ShowConstant h => h -> Column (PGRep h)
showConstant = constant
