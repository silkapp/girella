module Silk.Opaleye.Range where

import Opaleye.QueryArr
import qualified Opaleye.Order as O

type Range = (Offset, Limit)

mrange :: Maybe Range -> Query a -> Query a
mrange = maybe id range

range :: Range -> Query a -> Query a
range (Offset o, Limit l) = O.offset o . O.limit l

getOffsetValue :: Range -> Int
getOffsetValue = unOffset . fst

getLimitValue :: Range -> Int
getLimitValue = unLimit .snd

newtype Offset = Offset { unOffset :: Int }
  deriving (Eq, Show)

offset :: Offset -> Query a -> Query a
offset = O.limit . unOffset

moffset :: Maybe Offset -> Query a -> Query a
moffset = maybe id offset

newtype Limit = Limit { unLimit :: Int }
  deriving (Eq, Show)

limit :: Limit -> Query a -> Query a
limit = O.limit . unLimit

mlimit :: Maybe Limit -> Query a -> Query a
mlimit = maybe id limit
