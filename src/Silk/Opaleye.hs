{-# LANGUAGE
    Arrows
  , DefaultSignatures
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
module Silk.Opaleye
  ( module Control.Arrow
  , module Silk.Opaleye
  , module Silk.Opaleye.Compat
  , module Silk.Opaleye.Transaction
  , module Silk.Opaleye.TypeFams
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
  , Range
  , Query
  , QueryArr
  , Column
  , ShowConstant (..)
  , aggregate
  , asc
  , boolOr
  , constant
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
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Int (Int64)
import Safe
import qualified Data.List as L (sum)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import Data.Profunctor.Product.Default
import Opaleye.Aggregate
import Opaleye.Column
import Opaleye.Internal.Column (constant)
import Opaleye.Join (leftJoin)
import Opaleye.Manipulation (Unpackspec)
import Opaleye.Operators
import Opaleye.Order
import Opaleye.QueryArr
import Opaleye.RunQuery (QueryRunner)
import Opaleye.Table
import qualified Opaleye.Column       as C
import qualified Opaleye.Manipulation as M (runDelete, runInsert, runInsertReturning, runUpdate)
import qualified Opaleye.Operators    as O
import qualified Opaleye.RunQuery     as M (runQuery)

import Silk.Opaleye.Compat
import Silk.Opaleye.Instances ()
import Silk.Opaleye.Range (Range)
import Silk.Opaleye.TH
import Silk.Opaleye.Transaction
import Silk.Opaleye.TypeFams
import qualified Silk.Opaleye.Range as Range

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
  unsafeIOToQ $ M.runUpdate conn tab upd cond

-- | Update without using the current values
runUpdateConst :: Transaction m => Table columnsW columnsR -> columnsW -> (columnsR -> Column Bool) -> m Int64
runUpdateConst tab me cond = runUpdate tab (const me) cond

runDelete :: Transaction m => Table a columnsR -> (columnsR -> Column Bool) -> m Int64
runDelete tab e = liftQ $ do
  conn <- ask
  unsafeIOToQ $ M.runDelete conn tab e

-- | Run a query and convert the result using Conv.
runQuery :: ( Default QueryRunner columns haskells
             , haskells ~ OpaRep domain
             , Conv domain
             , Transaction m
             ) => Query columns -> m [domain]
runQuery = fmap conv . runQueryInternal

-- | Same as 'queryConv' but only fetches the first row.
runQueryFirst :: (Default QueryRunner columns (OpaRep domain), Conv domain, Transaction m) => Query columns -> m (Maybe domain)
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
where_ :: (b -> Column Bool) -> QueryArr b b
where_ p = restrict . arr p *> id

whereEq :: (b1 -> Column b0) -> (b2 -> Column b0) -> QueryArr (b1,b2) (b1,b2)
whereEq f g = where_ (arr (f . fst) .==. arr (g . snd))

innerJoin :: Query columnsB -> (columnsB -> Column a) -> (columnsA -> Column a) -> QueryArr columnsA columnsB
innerJoin queryB joinB joinA =
  proc columnsA -> do
    columnsB <- queryB -< ()
    restrict . arr (\(a,b) -> joinA a .== joinB b) -< (columnsA, columnsB)
    id -< columnsB

-- Query helpers

ors :: Foldable f => f (Column Bool) -> Column Bool
ors = foldr (.||) (constant False)

ands :: Foldable f => f (Column Bool) -> Column Bool
ands = foldr (.&&) (constant True)

inE :: ShowConstant h => [Column h] -> Column h -> Column Bool
inE hs w = ors . map (w .==) $ hs

notIn :: ShowConstant h => [Column h] -> Column h -> Column Bool
notIn hs w = ands . map (w ./=) $ hs

mrange :: Maybe Range -> Query a -> Query a
mrange = maybe id range

range :: Range -> Query a -> Query a
range r = offset (Range.offset r) . limit (Range.limit r)

-- | Specialized to use in aggregations, just using 'Data.List.sum' may make the type ambiguous.
sumInt64 :: [Int64] -> Int64
sumInt64 = L.sum

-- Avoiding clashes with prelude

eNot :: Column Bool -> Column Bool
eNot = O.not

eNull :: Column (Nullable a)
eNull = C.null

-- Implicit calls to ShowConstant

nullable :: ShowConstant a => a -> Column (Nullable a)
nullable = toNullable . constant

constantE :: ShowConstant a => a -> x -> Column a
constantE = const . constant

maybeToNullable :: ShowConstant a => Maybe a -> Column (Nullable a)
maybeToNullable = maybe eNull nullable

fromNullable :: ShowConstant a => a -> Column (Nullable a) -> Column a
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
