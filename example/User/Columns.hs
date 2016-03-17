{-# LANGUAGE
    DeriveDataTypeable
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module User.Columns
  ( Id (..)
  , Gender (..)
  ) where

-- Per convention we use 'id' and '(.)' 'from Control.Category' If you
-- don't want this, use 'returnA' and '<<<' respectively instead.

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID

import Silk.Opaleye.TH

-- | We use a newtype to reperesent a foreign key
newtype Id = Id { unId :: UUID }
  deriving (Show, Typeable)

-- | This generates the following instances for our type:
--
-- To convert from a postgresql type to Id:
-- > instance FromField Id
--
-- To convert from Id to an opaleye value:
-- > instance ShowConstant Id where
-- >   type PGRep Id = PGRep UUID
--
-- To convert from our type in an opaleye context to the haskell value:
-- > instance PGRep Id ~ a => QueryRunnerColumnDefault Id Id
--
-- Converting to an Id from an opaleye value (for Id this is the identity).
-- > instance Conv Id
--
-- We can look at values on several levels
--
-- 1. As a normal haskell value, represented by UUID. The
-- representation only differs by a newtype (e.g. on the type level)
-- so we can convert between them.
--
-- 2. As an opaleye value (inside queries), here 'Id' has a
-- representation PGRep Id = PGRep UUID = PGUuid. Each level has a
-- different type so that different keys can't be mixed, if we really
-- want to mix them up we can use 'unsafeCoerceColumn' (or safer
-- variants that we provide)
--
-- 3. As a database value, using postgresql's uuid type.
mkId ''Id

-- If we don't have a simple newtype we need a bit more boiler plate:
-- Converting between our types and some other type opaleye already
-- supports.
--
-- Here we have an enum so we can easily convert that to and from a
-- String (or other textual types). It could also be possible to use
-- an enum in postgres for this, but at the current time I don't think
-- opaleye supports it.
--
-- This gives a partial conversion when we go from the database to our
-- type so you should take care that this may need to be migrated if
-- changed.
data Gender = Male | Female
  deriving (Show, Typeable)

genderToText :: Gender -> Text
genderToText = \case
  Male   -> "male"
  Female -> "female"

textToGender :: Text -> Maybe Gender
textToGender = \case
  "male"   -> Just Male
  "female" -> Just Female
  _        -> Nothing


-- | The 'makeId' call is a more specific version of
-- 'makeColumnInstances' that supports any column-to-value
-- mapping. For Gender this generates:
--
-- > instance FromField Gender
--
-- To convert from Gender to a database value:
-- > instance ShowConstant Gender where
-- >   type PGRep Gender = PGRep String
--
-- To convert from our type opaleye context to the haskell value:
-- > instance PGRep Gender ~ a => QueryRunnerColumnDefault Gender Gender
--
-- Converting to a Gender from an opaleye value (this is the identity)
-- > instance Conv Gender
--
makeColumnInstances ''Gender ''Text 'genderToText 'textToGender
