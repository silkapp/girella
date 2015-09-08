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
import qualified Opaleye.Join      as J (leftJoin)
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

leftJoin ::
  ( Default Unpackspec columnsA columnsA
  , Default Unpackspec columnsB columnsB
  , Default NullMaker columnsB nullableColumnsB
  )
  => Query columnsA -> Query columnsB
  -> ((columnsA, columnsB) -> Column Bool)
  -> Query (columnsA, nullableColumnsB)
leftJoin a b f = J.leftJoin a b (safeCoerceToRep . f)

restrict :: QueryArr (Column Bool) ()
restrict = O.restrict . arr safeCoerceToRep
