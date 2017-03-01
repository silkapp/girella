{-# LANGUAGE FlexibleContexts #-}
module Silk.Opaleye.Misc
  ( fst3
  , snd3
  , trd3
  , fst4
  , snd4
  , trd4
  , fth4
  , fst5
  , snd5
  , trd5
  , fth5
  , fft5
  , flatten3
  , uuidText
  , sumInt64
  , showSql
  ) where

import Data.Maybe (fromMaybe)
import Data.Int (Int64)
import Data.Profunctor.Product.Default
import Data.String.Conversions
import Data.UUID (UUID)
import Opaleye.Manipulation (Unpackspec)
import Opaleye.QueryArr
import Opaleye.Sql (showSqlForPostgres)
import qualified Data.UUID as UUID (toString)

import qualified Data.List as L (sum)

-- | Turn a Query into a String, for debugging
showSql :: Default Unpackspec columns columns => Query columns -> String
showSql = fromMaybe "Query will return no rows" . showSqlForPostgres

flatten3 :: ((a, b), c) -> (a, b, c)
flatten3 ((x, y), z) = (x, y, z)

uuidText :: UUID -> StrictText
uuidText = cs . UUID.toString

-- | Specialized to use in aggregations, just using 'Data.List.sum' may make the type ambiguous.
sumInt64 :: [Int64] -> Int64
sumInt64 = L.sum

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a
snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b
trd4 :: (a,b,c,d) -> c
trd4 (_,_,c,_) = c
fth4 :: (a,b,c,d) -> d
fth4 (_,_,_,d) = d

fst5 :: (a,b,c,d,e) -> a
fst5 (a,_,_,_,_) = a
snd5 :: (a,b,c,d,e) -> b
snd5 (_,b,_,_,_) = b
trd5 :: (a,b,c,d,e) -> c
trd5 (_,_,c,_,_) = c
fth5 :: (a,b,c,d,e) -> d
fth5 (_,_,_,d,_) = d
fft5 :: (a,b,c,d,e) -> e
fft5 (_,_,_,_,e) = e
