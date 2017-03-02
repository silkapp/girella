{-# LANGUAGE
    DefaultSignatures
  , FlexibleInstances
  , TypeFamilies
  , TypeSynonymInstances
  #-}
module Girella.Conv (Conv (..)) where

import Data.CaseInsensitive
import Data.Int
import Data.String.Conversions
import Data.Time
import Data.UUID

-- | Convert an opaleye result to a separate type. Conv is used by the
-- query functions.
--
-- Opaleye results are Haskell values, usually a tuple of table types.
--
-- Using the identity conversion we get just this value, good if you
-- have short lived objects or are writing very DB centric code.
--
-- Otherwise you may want to write a conversion to a "domain" type.
--
-- It's possible to write functions that can map to several haskell
-- types, compare Girella's query functions to opaleye's.
class Conv d where
  type OpaRep d :: *
  type OpaRep d = d
  conv :: OpaRep d -> d
  default conv :: (OpaRep d ~ d) => OpaRep d -> d
  conv = id

instance Conv Char
instance Conv (Maybe Char)
instance Conv StrictText
instance Conv (Maybe StrictText)
instance Conv LazyText
instance Conv (Maybe LazyText)
instance Conv Int
instance Conv (Maybe Int)
instance Conv Int64
instance Conv (Maybe Int64)
instance Conv Double
instance Conv (Maybe Double)
instance Conv Bool
instance Conv (Maybe Bool)
instance Conv UUID
instance Conv (Maybe UUID)
instance Conv Day
instance Conv (Maybe Day)
instance Conv UTCTime
instance Conv (Maybe UTCTime)
instance Conv LocalTime
instance Conv (Maybe LocalTime)
instance Conv TimeOfDay
instance Conv (Maybe TimeOfDay)

instance Conv a => Conv (CI a)

instance Conv a => Conv [a] where
  type OpaRep [a] = [OpaRep a]
  conv = fmap conv

instance (Conv a, Conv b) => Conv (a, b) where
  type OpaRep (a, b) = (OpaRep a, OpaRep b)
  conv (a, b) = (conv a, conv b)

instance (Conv a, Conv b, Conv c) => Conv (a, b, c) where
  type OpaRep (a, b, c) = (OpaRep a, OpaRep b, OpaRep c)
  conv (a, b, c) = (conv a, conv b, conv c)

instance (Conv a, Conv b, Conv c, Conv d) => Conv (a, b, c, d) where
  type OpaRep (a, b, c, d) = (OpaRep a, OpaRep b, OpaRep c, OpaRep d)
  conv (a, b, c, d) = (conv a, conv b, conv c, conv d)

instance (Conv a, Conv b, Conv c, Conv d, Conv e) => Conv (a, b, c, d, e) where
  type OpaRep (a, b, c, d, e) = (OpaRep a, OpaRep b, OpaRep c, OpaRep d, OpaRep e)
  conv (a, b, c, d, e) = (conv a, conv b, conv c, conv d, conv e)

instance (Conv a, Conv b, Conv c, Conv d, Conv e, Conv f) => Conv (a, b, c, d, e, f) where
  type OpaRep (a, b, c, d, e, f) = (OpaRep a, OpaRep b, OpaRep c, OpaRep d, OpaRep e, OpaRep f)
  conv (a, b, c, d, e, f) = (conv a, conv b, conv c, conv d, conv e, conv f)

instance (Conv a, Conv b, Conv c, Conv d, Conv e, Conv f, Conv g) => Conv (a, b, c, d, e, f, g) where
  type OpaRep (a, b, c, d, e, f, g) = (OpaRep a, OpaRep b, OpaRep c, OpaRep d, OpaRep e, OpaRep f, OpaRep g)
  conv (a, b, c, d, e, f, g) = (conv a, conv b, conv c, conv d, conv e, conv f, conv g)

instance (Conv a, Conv b, Conv c, Conv d, Conv e, Conv f, Conv g, Conv h) => Conv (a, b, c, d, e, f, g, h) where
  type OpaRep (a, b, c, d, e, f, g, h) = (OpaRep a, OpaRep b, OpaRep c, OpaRep d, OpaRep e, OpaRep f, OpaRep g, OpaRep h)
  conv (a, b, c, d, e, f, g, h) = (conv a, conv b, conv c, conv d, conv e, conv f, conv g, conv h)
