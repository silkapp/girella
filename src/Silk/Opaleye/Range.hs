module Silk.Opaleye.Range
  ( Range
  , mrange
  , range

  , Offset (..)
  , moffset
  , offset
  , getOffset

  , Limit (..)
  , mlimit
  , limit
  , getLimit

  , DateSlice
  , dateSlice

  , After (..)
  , mafter
  , after

  , Before (..)
  , mbefore
  , before
  ) where

import Prelude hiding (id, (.))

import Data.Time

import Control.Category (id, (.))
import Opaleye.Column (Column)
import Opaleye.QueryArr
import qualified Opaleye.Order as O

import Silk.Opaleye.Combinators
import Silk.Opaleye.Operators
import Silk.Opaleye.ShowConstant

type Range = (Offset, Limit)

mrange :: Maybe Range -> Query a -> Query a
mrange = maybe id range

range :: Range -> Query a -> Query a
range (Offset o, Limit l) = O.offset o . O.limit l


newtype Offset = Offset { unOffset :: Int }
  deriving (Eq, Show)

moffset :: Maybe Offset -> Query a -> Query a
moffset = maybe id offset

offset :: Offset -> Query a -> Query a
offset = O.offset . unOffset

getOffset :: Range -> Int
getOffset = unOffset . fst


newtype Limit = Limit { unLimit :: Int }
  deriving (Eq, Show)

mlimit :: Maybe Limit -> Query a -> Query a
mlimit = maybe id limit

limit :: Limit -> Query a -> Query a
limit = O.limit . unLimit

getLimit :: Range -> Int
getLimit = unLimit . snd


type DateSlice = (Maybe After, Maybe Before)

dateSlice :: (a -> Column UTCTime) -> DateSlice -> QueryArr a a
dateSlice dateCol (a,b) = mbefore dateCol b . mafter dateCol a


newtype After = After { unAfter :: UTCTime }
  deriving (Eq, Show)

mafter ::  (a -> Column UTCTime) -> Maybe After -> QueryArr a a
mafter dateCol = maybe id (after dateCol)

after :: (a -> Column UTCTime) -> After -> QueryArr a a
after dateCol (After t) = where_ (\e -> dateCol e .> constant t)


newtype Before = Before { unBefore :: UTCTime }
  deriving (Eq, Show)

mbefore :: (a -> Column UTCTime) -> Maybe Before -> QueryArr a a
mbefore dateCol = maybe id (before dateCol)

before :: (a -> Column UTCTime) -> Before -> QueryArr a a
before dateCol (Before t) = where_ (\e -> dateCol e .< constant t)
