{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  #-}
module Silk.Opaleye.Query
  ( runInsert
  , runInsertReturning
  , runUpdate
  , runUpdateConst
  , runDelete
  , runQueryInternal
  , runQuery
  , runQueryFirst
  ) where

import Control.Monad.Reader
import Data.Int (Int64)
import Data.Profunctor.Product.Default
import Safe

import Opaleye.Manipulation (Unpackspec)
import Opaleye.QueryArr
import Opaleye.RunQuery (QueryRunner)
import Opaleye.Table
import qualified Opaleye.Manipulation as M (runDelete, runInsert, runInsertReturning, runUpdate)
import qualified Opaleye.RunQuery     as M (runQuery)

import Silk.Opaleye.Conv
import Silk.Opaleye.ShowConstant
import Silk.Opaleye.TH
import Silk.Opaleye.Transaction

-- | runInsert inside a Transaction
runInsert :: Transaction m => Table columns columns' -> columns -> m ()
runInsert tab q = liftQ $ do
  conn <- ask
  unsafeIOToTransaction . void $ M.runInsert conn tab q

-- | runInsertReturning inside a Transaction
runInsertReturning
  :: ( Default QueryRunner returned haskells
     , Default Unpackspec returned returned
     , OpaRep domain ~ haskells
     , Conv domain
     , Transaction m
     )
  => Table columnsW columnsR
  -> columnsW
  -> (columnsR -> returned)
  -> m [domain]
runInsertReturning tab ins ret = liftQ $ do
  conn <- ask
  fmap conv $ unsafeIOToTransaction $ M.runInsertReturning conn tab ins ret

-- | runUpdate inside a Transaction
runUpdate :: Transaction m => Table columnsW columnsR -> (columnsR -> columnsW) -> (columnsR -> Column Bool) -> m Int64
runUpdate tab upd cond = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ M.runUpdate conn tab upd (safeCoerceToRep . cond)

-- | Update without using the current values
runUpdateConst :: Transaction m => Table columnsW columnsR -> columnsW -> (columnsR -> Column Bool) -> m Int64
runUpdateConst tab = runUpdate tab . const

runDelete :: Transaction m => Table a columnsR -> (columnsR -> Column Bool) -> m Int64
runDelete tab cond = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ M.runDelete conn tab (safeCoerceToRep . cond)

-- | Opaleye's runQuery inside a Transaction, does not use 'Conv'
runQueryInternal
  :: ( Default QueryRunner columns haskells
     , Transaction m
     )
  => Query columns -> m [haskells]
runQueryInternal q = liftQ  $ do
  conn <- ask
  unsafeIOToTransaction $ M.runQuery conn q

-- | Run a query and convert the result using Conv.
runQuery :: ( Default QueryRunner columns haskells
            , Default Unpackspec columns columns
            , haskells ~ OpaRep domain
            , Conv domain
            , Transaction m
            ) => Query columns -> m [domain]
runQuery q =
-- Useful to uncomment when debugging query errors.
-- unsafeIOToTransaction . putStrLn . showSqlForPostgres $ q
  fmap conv . runQueryInternal $ q

-- | Same as 'queryConv' but only fetches the first row.
runQueryFirst :: ( Default Unpackspec columns columns
                 , Default QueryRunner columns (OpaRep domain)
                 , Conv domain
                 , Transaction m
                 ) => Query columns -> m (Maybe domain)
runQueryFirst = fmap headMay . runQuery
