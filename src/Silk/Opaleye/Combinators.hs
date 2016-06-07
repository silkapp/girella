{-# LANGUAGE
    Arrows
  , FlexibleContexts
  , NoImplicitPrelude
  #-}
module Silk.Opaleye.Combinators
  ( where_
  , whereEq
  , innerJoinOn
  , leftJoin
  , leftJoinExplicit
  , rightJoin
  , rightJoinExplicit
  , fullJoin
  , fullJoinExplicit
  -- * Re-exports
  , restrict
  ) where

import Prelude.Compat hiding (id, (.))

import Control.Arrow (arr)
import Control.Category (id, (.))
import Data.Profunctor.Product.Default

import Opaleye.Internal.Join (NullMaker)
import Opaleye.Manipulation (Unpackspec)
import Opaleye.QueryArr
import qualified Opaleye.Join      as J
import qualified Opaleye.Operators as O

import Silk.Opaleye.Operators
import Silk.Opaleye.ShowConstant
import Silk.Opaleye.TH

-- | Composable version of restrict
-- > where_ (\(c,u) -> Comment.userId c .== User.id' u) . (queryComment &&& queryUser)
where_ :: (a -> Column Bool) -> QueryArr a a
where_ p = restrict . arr p *> id

-- | Version of 'where_' only comparing equality
-- > whereEq Comment.userId User.id' . (queryComment &&& queryUser)
whereEq :: ShowConstant a => (t -> Column a) -> (t1 -> Column a) -> QueryArr (t, t1) (t, t1)
whereEq f g = where_ (\(a,b) -> f a .== g b)

-- | Typical usage:
-- > comment <- Comment.queryAll `innerJoinOn` Comment.userId -< User.id' user
innerJoinOn :: QueryArr () a -> (a -> Column b) -> QueryArr (Column b) a
innerJoinOn q column = proc a -> do
  q' <- q -< ()
  O.restrict -< column q' O..== a
  id -< q'

infixr 4 `innerJoinOn`

-- | A normal SQL left join.
--
-- Usually it's nicest to do left joins first out of joins in a query.
leftJoinExplicit
  :: Unpackspec columnsA columnsA
  -> Unpackspec columnsB columnsB
  -> NullMaker columnsB nullableColumnsB
  -> Query columnsA
  -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (columnsA, nullableColumnsB)
leftJoinExplicit unA unB nulB a b f = J.leftJoinExplicit unA unB nulB a b (safeCoerceToRep . f)

rightJoinExplicit
  :: Unpackspec columnsA columnsA
  -> Unpackspec columnsB columnsB
  -> NullMaker columnsA nullableColumnsA
  -> Query columnsA
  -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (nullableColumnsA, columnsB)
rightJoinExplicit unA unB nulA a b f = J.rightJoinExplicit unA unB nulA a b (safeCoerceToRep . f)

fullJoinExplicit
  :: Unpackspec columnsA columnsA
  -> Unpackspec columnsB columnsB
  -> NullMaker columnsA nullableColumnsA
  -> NullMaker columnsB nullableColumnsB
  -> Query columnsA
  -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (nullableColumnsA, nullableColumnsB)
fullJoinExplicit unA unB nulA nulB a b f = J.fullJoinExplicit unA unB nulA nulB a b (safeCoerceToRep . f)

leftJoin ::
  ( Default Unpackspec columnsA columnsA
  , Default Unpackspec columnsB columnsB
  , Default NullMaker columnsB nullableColumnsB
  )
  => Query columnsA
  -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (columnsA, nullableColumnsB)
leftJoin = leftJoinExplicit def def def

rightJoin ::
  ( Default Unpackspec columnsA columnsA
  , Default Unpackspec columnsB columnsB
  , Default NullMaker columnsA nullableColumnsA
  )
  => Query columnsA
  -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (nullableColumnsA, columnsB)
rightJoin = rightJoinExplicit def def def

fullJoin ::
  ( Default Unpackspec columnsA columnsA
  , Default Unpackspec columnsB columnsB
  , Default NullMaker columnsA nullableColumnsA
  , Default NullMaker columnsB nullableColumnsB
  )
  => Query columnsA
  -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (nullableColumnsA, nullableColumnsB)
fullJoin = fullJoinExplicit def def def def

-- | Opaleye's 'restrict' over a 'Bool'.
restrict :: QueryArr (Column Bool) ()
restrict = O.restrict . arr safeCoerceToRep
