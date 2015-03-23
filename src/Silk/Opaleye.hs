{-# LANGUAGE
    Arrows
  , DefaultSignatures
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , LambdaCase
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
module Silk.Opaleye
  ( module Control.Arrow
  , module Silk.Opaleye
  , module Silk.Opaleye.Range
  , module Silk.Opaleye.Transaction
  , module Silk.Opaleye.TypeFams
  , module Opaleye.PGTypes
  , (.&&)
  , (.==)
  , (./=)
  , (.||)
  , (.>)
  , (.<)
  , (.>=)
  , (.<=)
  , Connection
  , Int64
  , Nullable
  , Order
  , Pool
  , Query
  , QueryArr
  , Column
  , ShowConstant (..)
  , aggregate
  , asc
  , boolOr
  , contramap
  , count
  , desc
  , fieldQueryRunnerColumn
  , groupBy
  , isNull
  , leftJoin
  , lmap
  , lower
  , matchNullable
  , orderBy
  , queryTable
  , required
  , toNullable
  , unsafeCoerce
  ) where

import Prelude hiding (foldr, id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category (id, (.))
import Control.Monad.Reader
import Data.Foldable
import Data.Functor.Contravariant (Contravariant (..))
import Data.Pool
import Data.Profunctor
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Int (Int64)
import Opaleye.Sql (showSqlForPostgres)
import Safe
import qualified Data.List as L (sum)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import Data.Profunctor.Product.Default
import Opaleye.Aggregate
import Opaleye.Column
import Opaleye.Join (leftJoin)
import Opaleye.Manipulation (Unpackspec)
import Opaleye.Operators
import Opaleye.Order (Order, asc, desc, orderBy)
import Opaleye.PGTypes
import Opaleye.QueryArr
import Opaleye.RunQuery (QueryRunner)
import Opaleye.Table
import qualified Opaleye.Column       as C
import qualified Opaleye.Manipulation as M (runDelete, runInsert, runInsertReturning, runUpdate)
import qualified Opaleye.Operators    as O
import qualified Opaleye.RunQuery     as M (runQuery)

import Silk.Opaleye.Range
import Silk.Opaleye.TH
import Silk.Opaleye.Transaction
import Silk.Opaleye.TypeFams

runInsert :: Transaction m => Table columns columns' -> columns -> m ()
runInsert tab q = liftQ . Q $ do
  conn <- ask
  liftIO $ void $ M.runInsert conn tab q

runInsertReturning
  :: ( Default QueryRunner returned haskells
     , Default Unpackspec returned returned
     , Transaction m
     )
  => Table columnsW columnsR
  -> columnsW
  -> (columnsR -> returned)
  -> m [haskells]
runInsertReturning tab ins ret = liftQ . Q $ do
  conn <- ask
  liftIO $ M.runInsertReturning conn tab ins ret

runQueryInternal
  :: ( Default QueryRunner columns haskells
     , Transaction m
     )
  => Query columns -> m [haskells]
runQueryInternal q = liftQ . Q $ do
  conn <- ask
  liftIO $ M.runQuery conn $ q

runUpdate :: Transaction m => Table columnsW columnsR -> (columnsR -> columnsW) -> (columnsR -> Column PGBool) -> m Int64
runUpdate tab upd cond = liftQ $ do
  conn <- ask
  unsafeIOToQ $ M.runUpdate conn tab upd cond

-- | Update without using the current values
runUpdateConst :: Transaction m => Table columnsW columnsR -> columnsW -> (columnsR -> Column PGBool) -> m Int64
runUpdateConst tab me cond = runUpdate tab (const me) cond

runDelete :: Transaction m => Table a columnsR -> (columnsR -> Column PGBool) -> m Int64
runDelete tab e = liftQ $ do
  conn <- ask
  unsafeIOToQ $ M.runDelete conn tab e

-- | Run a query and convert the result using Conv.
runQuery :: ( Default QueryRunner columns haskells
            , Default Unpackspec columns columns
            , haskells ~ OpaRep domain
            , Conv domain
            , Transaction m
            ) => Query columns -> m [domain]
runQuery q = do
-- Useful to uncomment when debugging query errors.
-- unsafeIOToTransaction . putStrLn . showSqlForPostgres $ q
  fmap conv . runQueryInternal $ q

showSql :: Default Unpackspec columns columns => Query columns -> String
showSql = showSqlForPostgres

-- | Same as 'queryConv' but only fetches the first row.
runQueryFirst :: ( Default Unpackspec columns columns
                 , Default QueryRunner columns (OpaRep domain)
                 , Conv domain
                 , Transaction m
                 ) => Query columns -> m (Maybe domain)
runQueryFirst = fmap headMay . runQuery

-- | Convert an opaleye result to a separate type.
--
-- Also has a default implementation for the identity conversion so
-- both domain and database functions can use query functions in
-- the same way.
class Conv d where
  type OpaRep d :: *
  type OpaRep d = d
  conv :: OpaRep d -> d
  default conv :: (OpaRep d ~ d) => OpaRep d -> d
  conv = id

instance Conv a => Conv [a] where
  type OpaRep [a] = [OpaRep a]
  conv = fmap conv

-- | Composable version of restrict
where_ :: (b -> Column PGBool) -> QueryArr b b
where_ p = restrict . arr p *> id

whereEq :: (b1 -> Column b0) -> (b2 -> Column b0) -> QueryArr (b1,b2) (b1,b2)
whereEq f g = where_ (\(a,b) -> f a .== g b)

{-# DEPRECATED innerJoin "Use innerJoinOn" #-}
innerJoin :: Query columnsB -> (columnsB -> Column a) -> (columnsA -> Column a) -> QueryArr columnsA columnsB
innerJoin queryB joinB joinA =
  proc columnsA -> do
    columnsB <- queryB -< ()
    restrict . arr (\(a,b) -> joinA a .== joinB b) -< (columnsA, columnsB)
    id -< columnsB

innerJoinOn :: Query columnsA -> (columnsA -> Column a) -> QueryArr (Column a) columnsA
innerJoinOn q column = proc a -> do
  q' <- q -< ()
  restrict -< column q' .== a
  id -< q'

-- Query helpers

ors :: Foldable f => f (Column PGBool) -> Column PGBool
ors = foldr (.||) (constant False)

ands :: Foldable f => f (Column PGBool) -> Column PGBool
ands = foldr (.&&) (constant True)

inE :: [Column o] -> Column o -> Column PGBool
inE hs w = ors . map (w .==) $ hs

notIn :: [Column a] -> Column a -> Column PGBool
notIn hs w = ands . map (w ./=) $ hs

data DateSlice
  = Before UTCTime
  | After UTCTime
  deriving (Eq, Show)

dateSlice :: (a -> Column PGTimestamptz) -> Maybe DateSlice -> QueryArr a a
dateSlice dateCol mslice = case mslice of
  Nothing -> where_ (const $ constant True)
  Just (Before tb) -> where_ (\e -> dateCol e .< constant tb)
  Just (After  ta) -> where_ (\e -> dateCol e .> constant ta)

-- | Specialized to use in aggregations, just using 'Data.List.sum' may make the type ambiguous.
sumInt64 :: [Int64] -> Int64
sumInt64 = L.sum

-- Avoiding clashes with prelude

eNot :: Column PGBool -> Column PGBool
eNot = O.not

eNull :: Column (Nullable a)
eNull = C.null

-- Implicit calls to ShowConstant

nullable :: (ShowConstant a) => a -> Column (Nullable (PGRep a))
nullable = toNullable . constant

maybeToNullable :: (ShowConstant a, b ~ PGRep a) => Maybe a -> Column (Nullable b)
maybeToNullable = maybe eNull nullable

fromNullable :: (ShowConstant a, b ~ PGRep a) => a -> Column (Nullable b) -> Column b
fromNullable a c = C.fromNullable (constant a) c

-- Unsafe and pretty crappy

cast :: String -> Column a -> Column b
cast = unsafeCast

-- Misc

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

flatten3 :: ((a, b), c) -> (a, b, c)
flatten3 ((x, y), z) = (x, y, z)

uuidText :: UUID -> Text
uuidText = T.pack . UUID.toString
