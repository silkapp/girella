{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , TypeFamilies
  #-}
module Girella.Query
  ( runInsert
  , runInsertReturning
  , runUpdate
  , runUpdateEasy
  , runUpdateConst
  , runDelete
  , runQueryInternalExplicit
  , runQueryInternal
  , runQueryExplicit
  , runQuery
  , runQueryFirst
  , runQueryString
  , foldQueryInternalExplicit
  , foldQueryInternal
  , foldQueryExplicit
  , foldQuery
  ) where

import Control.Monad.Reader
import Data.Int (Int64)
import Data.Profunctor.Product.Default
import Data.String (IsString (fromString))
import GHC.Stack (CallStack, getCallStack)
import Safe
import qualified Database.PostgreSQL.Simple as PG

import Opaleye.Label (label)
import Opaleye.Manipulation (Unpackspec, Updater)
import Opaleye.QueryArr
import Opaleye.RunQuery (QueryRunner)
import Opaleye.Table
import qualified Opaleye.Manipulation as M (runDelete, runInsert, runInsertReturning, runUpdate, runUpdateEasy)
import qualified Opaleye.RunQuery     as M (runQueryExplicit, runQueryFoldExplicit)

import Girella.Compat (prettySrcLoc)
import Girella.Conv
import Girella.ShowConstant
import Girella.TH
import Girella.Transaction

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
  fmap conv . unsafeIOToTransaction $ M.runInsertReturning conn tab ins ret

-- | runUpdate inside a Transaction
runUpdate :: Transaction m => Table columnsW columnsR -> (columnsR -> columnsW) -> (columnsR -> Column Bool) -> m Int64
runUpdate tab upd cond = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ M.runUpdate conn tab upd (safeCoerceToRep . cond)

-- | Update without using the current values
runUpdateConst :: Transaction m => Table columnsW columnsR -> columnsW -> (columnsR -> Column Bool) -> m Int64
runUpdateConst tab = runUpdate tab . const

-- | runUpdateEasy inside a Transaction
runUpdateEasy
  :: ( Transaction m
     , Default Updater columnsR columnsW
     )
  => Table columnsW columnsR
  -> (columnsR -> columnsR)
  -> (columnsR -> Column Bool)
  -> m Int64
runUpdateEasy tab upd cond = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ M.runUpdateEasy conn tab upd (safeCoerceToRep . cond)

runDelete :: Transaction m => Table a columnsR -> (columnsR -> Column Bool) -> m Int64
runDelete tab cond = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ M.runDelete conn tab (safeCoerceToRep . cond)

-- | Opaleye's runQuery inside a Transaction, does not use 'Conv'
runQueryInternalExplicit
  :: ( ?loc :: CallStack
     , Transaction m
     )
  => QueryRunner columns haskells
  -> Query columns
  -> m [haskells]
runQueryInternalExplicit qr q = liftQ $ do
  conn <- ask
  unsafeIOToTransaction . M.runQueryExplicit qr conn $ label (ppCallStack ?loc) q

foldQueryInternalExplicit
  :: ( ?loc :: CallStack
     , Transaction m
     )
  => QueryRunner columns haskells
  -> Query columns
  -> a
  -> (a -> haskells -> IO a)
  -> m a
foldQueryInternalExplicit qr q e f = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ M.runQueryFoldExplicit qr conn (label (ppCallStack ?loc) q) e f

foldQueryInternal
  :: ( ?loc :: CallStack
     , Default QueryRunner columns haskells
     , Transaction m
     )
  => Query columns
  -> a
  -> (a -> haskells -> IO a)
  -> m a
foldQueryInternal = foldQueryInternalExplicit def

foldQueryExplicit
  :: ( ?loc :: CallStack
     , haskells ~ OpaRep domain
     , Conv domain
     , Transaction m
     )
  => QueryRunner columns haskells
  -> Query columns
  -> a
  -> (a -> domain -> IO a)
  -> m a
foldQueryExplicit qr q e f = foldQueryInternalExplicit qr q e (\x -> f x . conv)

foldQuery
  :: ( ?loc :: CallStack
     , Default QueryRunner columns haskells
     , haskells ~ OpaRep domain
     , Conv domain
     , Transaction m
     )
  => Query columns
  -> a
  -> (a -> domain -> IO a)
  -> m a
foldQuery = foldQueryExplicit def

ppCallStack :: CallStack -> String
ppCallStack = maybe "no call stack available" (prettySrcLoc . snd) . lastMay . getCallStack

-- | Opaleye's runQuery inside a Transaction, does not use 'Conv'
runQueryInternal
  :: ( ?loc :: CallStack
     , Default QueryRunner columns haskells
     , Transaction m
     )
  => Query columns
  -> m [haskells]
runQueryInternal = runQueryInternalExplicit def

-- | Run a query and convert the result using Conv.
runQueryExplicit
  :: ( ?loc :: CallStack
     , haskells ~ OpaRep domain
     , Conv domain
     , Transaction m
     )
  => QueryRunner columns haskells
  -> Query columns
  -> m [domain]
runQueryExplicit qr {- q -} =
-- Useful to uncomment when debugging query errors.
-- unsafeIOToTransaction . putStrLn . showSqlForPostgres $ q
  fmap conv . runQueryInternalExplicit qr

-- | Run a query and convert the result using Conv.
runQuery
  :: ( ?loc :: CallStack
     , Default QueryRunner columns haskells
     , haskells ~ OpaRep domain
     , Conv domain
     , Transaction m
     )
  => Query columns
  -> m [domain]
runQuery = runQueryExplicit def

-- | Same as 'queryConv' but only fetches the first row.
runQueryFirst :: ( ?loc :: CallStack
                 , Default Unpackspec columns columns
                 , Default QueryRunner columns (OpaRep domain)
                 , Conv domain
                 , Transaction m
                 ) => Query columns -> m (Maybe domain)
runQueryFirst = fmap headMay . runQuery

runQueryString :: (PG.ToRow params, PG.FromRow res, Transaction m) => String -> params -> m [res]
runQueryString q params = liftQ $ do
  conn <- ask
  unsafeIOToTransaction $ PG.query conn (fromString q) params
