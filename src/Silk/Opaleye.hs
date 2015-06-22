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
  , module Silk.Opaleye.Transaction
  , module Silk.Opaleye.TypeFams
  , module Silk.Opaleye.Range
  , module Opaleye.PGTypes
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
  , contramap
  , count
  , desc
  , fieldQueryRunnerColumn
  , groupBy
  , lmap
  , matchNullable
  , orderBy
  , queryTable
  , required
  , toNullable
  ) where

import Prelude hiding (foldr, id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category (id, (.))
import Control.Monad.Reader
import Data.Foldable
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int64)
import Data.Pool
import Data.Profunctor
import Data.Profunctor.Product.Default
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Opaleye.Sql (showSqlForPostgres)
import Safe
import qualified Data.List as L (sum)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import Opaleye.Aggregate (Aggregator, aggregate, count, groupBy)
import Opaleye.Column (matchNullable, toNullable, unsafeCast)
import Opaleye.Internal.Join (NullMaker)
import Opaleye.Manipulation (Unpackspec)
import Opaleye.Order (Order, asc, desc, orderBy)
import Opaleye.PGTypes
import Opaleye.QueryArr
import Opaleye.RunQuery (QueryRunner)
import Opaleye.Table
import qualified Opaleye.Aggregate    as A
import qualified Opaleye.Column       as C
import qualified Opaleye.Join         as J (leftJoin)
import qualified Opaleye.Manipulation as M (runDelete, runInsert, runInsertReturning, runUpdate)
import qualified Opaleye.Operators    as O
import qualified Opaleye.RunQuery     as M (runQuery)

import Silk.Opaleye.Range
import Silk.Opaleye.ShowConstant
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

runUpdate :: Transaction m => Table columnsW columnsR -> (columnsR -> columnsW) -> (columnsR -> Column Bool) -> m Int64
runUpdate tab upd cond = liftQ $ do
  conn <- ask
  unsafeIOToQ $ M.runUpdate conn tab upd (toPGBool . cond)

-- | Update without using the current values
runUpdateConst :: Transaction m => Table columnsW columnsR -> columnsW -> (columnsR -> Column Bool) -> m Int64
runUpdateConst tab me cond = runUpdate tab (const me) cond

runDelete :: Transaction m => Table a columnsR -> (columnsR -> Column Bool) -> m Int64
runDelete tab cond = liftQ $ do
  conn <- ask
  unsafeIOToQ $ M.runDelete conn tab (toPGBool . cond)

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
where_ :: (a -> Column Bool) -> QueryArr a a
where_ p = restrict . arr p *> id

whereEq :: ShowConstant a => (t -> Column a) -> (t1 -> Column a) -> QueryArr (t, t1) (t, t1)
whereEq f g = where_ (\(a,b) -> f a .== g b)

innerJoinOn :: QueryArr () a -> (a -> Column b) -> QueryArr (Column b) a
innerJoinOn q column = proc a -> do
  q' <- q -< ()
  O.restrict -< column q' O..== a
  id -< q'

leftJoin ::
  ( Default Unpackspec columnsA columnsA
  , Default Unpackspec columnsB columnsB
  , Default NullMaker columnsB nullableColumnsB
  )
  => Query columnsA -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (columnsA, nullableColumnsB)
leftJoin a b f = J.leftJoin a b (toPGBool . f)

-- Operators!

infix 4 .==
(.==) :: ShowConstant a => Column a -> Column a -> Column Bool
a .== b = fromPGBool $ a O..== b
infix 4 .==?
(.==?) :: ShowConstant a => Column (Nullable a) -> Column (Nullable a) -> Column Bool
a .==? b = fromPGBool $ a O..== b
infix 4 ./=
(./=) :: ShowConstant a => Column a -> Column a -> Column Bool
a ./= b = fromPGBool $ a O../= b

infixr 2 .||
(.||) :: Column Bool -> Column Bool -> Column Bool
a .|| b = fromPGBool $ toPGBool a O..|| toPGBool b
infixr 3 .&&
(.&&) :: Column Bool -> Column Bool -> Column Bool
a .&& b = fromPGBool $ toPGBool a O..&& toPGBool b

infix 4 .>
(.>) :: TOrd a => Column a -> Column a -> Column Bool
a .> b = fromPGBool $ a O..> b
infix 4 .<
(.<) :: TOrd a => Column a -> Column a -> Column Bool
a .< b = fromPGBool $ a O..< b
infix 4 .>=
(.>=) :: TOrd a => Column a -> Column a -> Column Bool
a .>= b = fromPGBool $ a O..>= b
infix 4 .<=
(.<=) :: TOrd a => Column a -> Column a -> Column Bool
a .<= b = fromPGBool $ a O..<= b

lower :: TString a => Column a -> Column a
lower = unsafeCoerce . O.lower . unsafeCoerce

restrict :: QueryArr (Column Bool) ()
restrict = O.restrict . arr unsafeCoerce

-- Query helpers


ors :: Foldable f => f (Column Bool) -> Column Bool
ors = foldr (.||) (constant False)

ands :: Foldable f => f (Column Bool) -> Column Bool
ands = foldr (.&&) (constant True)

inE :: ShowConstant o => [Column o] -> Column o -> Column Bool
inE hs w = ors . map (w .==) $ hs

notIn :: ShowConstant a => [Column a] -> Column a -> Column Bool
notIn hs w = ands . map (./= w) $ hs

data DateSlice
  = Before UTCTime
  | After UTCTime
  deriving (Eq, Show)

dateSlice :: (a -> Column UTCTime) -> Maybe DateSlice -> QueryArr a a
dateSlice dateCol mslice = case mslice of
  Nothing -> where_ (const $ constant True)
  Just (Before tb) -> where_ (\e -> dateCol e .< constant tb)
  Just (After  ta) -> where_ (\e -> dateCol e .> constant ta)

-- | Specialized to use in aggregations, just using 'Data.List.sum' may make the type ambiguous.
sumInt64 :: [Int64] -> Int64
sumInt64 = L.sum

-- Avoiding clashes with prelude

eNot :: Column Bool -> Column Bool
eNot = fromPGBool . O.not . toPGBool

eNull :: Column (Nullable a)
eNull = C.null

-- Implicit calls to ShowConstant

nullable :: ShowConstant a => a -> Column (Nullable a)
nullable = toNullable . constant

maybeToNullable :: ShowConstant a => Maybe a -> Column (Nullable a)
maybeToNullable = maybe eNull nullable

fromNullable :: ShowConstant a => a -> Column (Nullable a) -> Column a
fromNullable a c = C.fromNullable (constant a) c

-- Aggregations

boolOr :: Aggregator (Column Bool) (Column Bool)
boolOr = dimap toPGBool fromPGBool A.boolOr

boolAnd :: Aggregator (Column Bool) (Column Bool)
boolAnd = dimap toPGBool fromPGBool A.boolAnd

maxColumn :: Aggregator (Column a) (Column a)
maxColumn = A.max

minColumn :: Aggregator (Column a) (Column a)
minColumn = A.min

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

isNull :: Column (Nullable a) -> Column Bool
isNull = fromPGBool . C.isNull

