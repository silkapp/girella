{-# LANGUAGE
    Arrows
  , DeriveDataTypeable
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , NoImplicitPrelude
  , NoMonomorphismRestriction
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module User
  ( module User.Columns
  -- * Table
  , UserP (..)
  , User
  , UserH
  , table
  , emptyUpdate
  -- * Queries
  , queryAll
  , byId
  , allByName
  , insert
  , update
  , updateEasy
  -- * Ordering
  , nameOrder
  -- * Running queries
  , runById
  , insertAndSelectAll
  , runInsertAndSelectAll
  -- * Projection / Sub-view
  , UserNameViewP (..)
  , UserNameView
  , UserNameViewH
  , queryAllView
  , queryAllViewSlower
  ) where

-- Per convention we use 'id' and '(.)' 'from Control.Category' If you
-- don't want this, use 'returnA' and '<<<' respectively instead.

import Prelude.Compat hiding (id, (.))

import Control.Category
import Data.Text (Text)

import Girella
import Girella.TH

import User.Columns

-- Here we define a data type for our table, each field has a haskell
-- type that can map to opaleye types. This gives us the possibility
-- to have a many-to-one mapping between haskell types and opaleye
-- types. For example, we want @User.Id@ to be separate from @Post.Id@
-- even if they have the same database representation.
--
-- This generates two type aliases and one type:
--
-- You normally don't have to use the type 'UserP' at all.

-- > data UserP a b c d = User { id' :: a, name' :: b, age :: c, gender :: d }
--
-- The Opaleye type of the table, this is is what you'll use when writing queries.
-- Optional fields are 'Nullable' here since SQL doesn't have proper 'Maybe's.
--
-- > type User  = UserP Id String Int (Nullable Gender)
--
-- > type UserH = UserP Id String Int (Maybe Gender)
--
-- The Haskell type you get if running a query retuns a user, Nullable
-- is mapped to a Maybe so it's usable.
--
-- We also get a type family instance for 'To' which allows us to say
-- things like @To Maybe PersonH@ or @To Column Person@ to wrap each field in a type.
--
-- > type instance To typ (UserP a b c d) = UserP (typ a) (typ b) (typ c) (typ d)
--
-- We additionally generate 'instance Conv UserH' which lets us use
-- the Haskell result values directly with the girella query
-- functions.
--
makeTypes [d|
    data User = User
      { id'    :: Id
      , name   :: Text
      , age    :: Int
      , gender :: Nullable Gender
      } deriving Show
  |]

-- | This is a product-profunctor TH splice that opaleye needs, you
-- don't need to understand what this does to use girella or opaleye,
-- but feel free to dig in.
makeAdaptorAndInstance "pUser" ''UserP

-- | Here we make a table definition for our type by giving it the
-- table name, the product profunctor for our type, and our type
-- itself.
--
-- This currently makes all columns nullable for inserts and updates
-- since it's the most flexible option.
--
-- This will produce a `table' which is the only entry-point we have
-- for working with the table.
makeTable "people" 'pUser ''UserP

-- | The simplest query, @select * from users@
--
-- This query can be reused by all queries referencing the users
-- table. By convention we only allow one call to `queryTable` per
-- table to make it clear who /owns/ the schema.
--
-- @Query a = QueryArr () a@ means have a full-fledged query that
-- doesn't /need/ any more input to give results.
--
-- @To Column User@ uses the 'To' type family, we could have instead written
--
-- > Query (UserP (Column Id) (Column String) (Column Int) (Column (Nullable Gender))
--
-- but that gets old and we find it's much less readable once you have gotten used to 'To'.
queryAll :: Query (To Column User)
queryAll = queryTable table

-- | where conditions:
--
-- > select * from users where id = x
--
-- 'where_' is the most general way to do a comparison (more general
-- than opaleye's 'restrict'). It's natural to use if you write code
-- without arrow notation. Think of it like 'filter'. It can also be
-- used to do joins, see its specialization 'whereEq'.
--
-- In the where clause we write things much similar to the resulting
-- SQL where clause. We project columns (@id'@) from the current query
-- and compare them with each other (or with constants as in our case).
byId :: Id -> Query (To Column User)
byId i = where_ (\u -> id' u .== constant i) . queryAll

-- |
-- > order by lower(name) asc
nameOrder :: Order (To Column User)
nameOrder = asc (lower . name)

-- |
-- > select * from queries order by lower(name) asc;
allByName :: Query (To Column User)
allByName = orderBy nameOrder queryAll

-- Generally:
-- > :: (Transaction m, Conv domain, OpaRep domain ~ UserH) => UUID -> m (Maybe domain)
runById :: Transaction m => Id -> m (Maybe UserH)
runById = runQueryFirst . byId

insert :: Transaction m => Id -> Text -> Int -> Maybe Gender -> m ()
insert i n a mg =
  runInsert table psn
  where
    psn :: To Maybe (To Column User)
    psn = User
      { id'    = Just $ constant i
      , name   = Just $ constant n
      , age    = Just $ constant a
      , gender = Just $ maybeToNullable mg
      }

update :: Transaction m => Id -> Text -> Int -> m Bool
update i n a = (> 0) <$> runUpdate table upd condition
  where
    upd :: To Column User -> To Maybe (To Column User)
    upd p = p
      { id'    = Just $ id' p
      , name   = Just $ constant n
      , age    = Just $ constant a
      , gender = Just $ gender p
      }
    condition :: To Column User -> Column Bool
    condition p = id' p .== constant i

updateEasy :: Transaction m => Id -> Text -> Int -> m Bool
updateEasy i n a = (> 0) <$> runUpdateEasy table upd condition
  where
    upd :: To Column User -> To Column User
    upd p = p
      { id'    = id' p
      , name   = constant n
      , age    = constant a
      }
    condition :: To Column User -> Column Bool
    condition p = id' p .== constant i

-- Type sig can be generalized to Conv as above.
insertAndSelectAll :: Transaction m => Id -> Text -> Int -> Maybe Gender -> m [UserH]
insertAndSelectAll i n a mg = do
  insert i n a mg
  runQuery queryAll

-- Usually no point in defining this function by itself, but it could form a larger transaction.
runInsertAndSelectAll :: MonadPool m => Id -> Text -> Int -> Maybe Gender -> m [UserH]
runInsertAndSelectAll i n a mg = runTransaction $ insertAndSelectAll i n a mg

-- We can define a projection/sub-view of a table by simply defining a smaller data type and re-using the entire table definition.
--
-- Is it efficient to base our sub view query on the entire User
-- table?
-- The generated SQL will be of the form
--
-- >>> select id, name from (select * from users)
--
-- but Postgres will optimize this to exclude the columns we aren't
-- using, you can verify this with an EXPLAIN ANALYZE.
--
-- If we instead would simply map @UserH -> MyUserNameView@ outside of 'QueryArr' postgres would
makeTypes [d|
    data UserNameView = UserNameView
      { idView   :: Id
      , nameView :: Text
      } deriving Show
  |]

makeAdaptorAndInstance "pUserNameView" ''UserNameViewP

-- Helper to allow us to make a view query out of any normal User
-- query.
--
-- Note that exporting this may encourage to run queries such as in
-- 'queryAllViewSlower' so we've left it un-exposed.
toView :: UserP a b c d -> UserNameViewP a b
toView u = UserNameView { idView = id' u, nameView = name u }

queryAllView :: Query (To Column UserNameView)
queryAllView = toView <$> queryAll

-- Doing the query this way is not composable as we are in
-- 'Transaction'. It's also less efficient as the mapping from the
-- normal User type happens outside of Postgres.
queryAllViewSlower :: Transaction m => m [UserNameViewH]
queryAllViewSlower = do
  r :: [UserH] <- runQuery queryAll
  pure $ map toView r
