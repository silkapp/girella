module Silk.Opaleye.Range where

import Prelude hiding ((.))

import Data.Time

import Control.Category ((.))
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
offset = O.limit . unOffset

getOffsetValue :: Range -> Int
getOffsetValue = unOffset . fst


newtype Limit = Limit { unLimit :: Int }
  deriving (Eq, Show)

mlimit :: Maybe Limit -> Query a -> Query a
mlimit = maybe id limit

limit :: Limit -> Query a -> Query a
limit = O.limit . unLimit

getLimitValue :: Range -> Int
getLimitValue = unLimit . snd


type DateSlice = (After, Before)

dateSlice :: (a -> Column UTCTime) -> (After, Before) -> QueryArr a a
dateSlice dateCol (a,b) = before dateCol b . after dateCol a


newtype After = After { unAfter :: UTCTime }
  deriving (Eq, Show)

after :: (a -> Column UTCTime) -> After -> QueryArr a a
after dateCol (After t) = where_ (\e -> dateCol e .> constant t)


newtype Before = Before { unBefore :: UTCTime }
  deriving (Eq, Show)

before :: (a -> Column UTCTime) -> Before -> QueryArr a a
before dateCol (Before t) = where_ (\e -> dateCol e .< constant t)
