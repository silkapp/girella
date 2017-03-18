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
module Article
  ( module Article.Columns
  -- * Table
  , ArticleP (..)
  , Article
  , ArticleH
  , table
  , emptyUpdate
  -- * Queries
  , queryAll
  , byId
  , allByTitle
  , insert
  , update
  , updateEasy
  -- * Joins with User
  , articlesByUserName
  -- * Ordering
  , titleOrder
  -- * Running queries
  , runById
  ) where

-- Per convention we use 'id' and '(.)' 'from Control.Category' If you
-- don't want this, use 'returnA' and '<<<' respectively instead.

import Prelude.Compat hiding (id, (.))

import Control.Category
import Data.Text (Text)

import Girella
import Girella.TH

import Article.Columns
import User (User)
import qualified User

makeTypes [d|
    data Article = Article
      { id'      :: Id
      , authorId :: User.Id
      , title    :: Text
      , content  :: Text
      } deriving Show
  |]

makeAdaptorAndInstance "pArticle" ''ArticleP

makeTable "articles" 'pArticle ''ArticleP

queryAll :: Query (To Column Article)
queryAll = queryTable table

byId :: Id -> Query (To Column Article)
byId i = where_ (\u -> id' u .== constant i) . queryAll

titleOrder :: Order (To Column Article)
titleOrder = asc (lower . title)

allByTitle :: Query (To Column Article)
allByTitle = orderBy titleOrder queryAll

articlesByUserName :: Query (MapTo Column (User, Article))
articlesByUserName = proc () -> do
  users <- User.allByName -< ()
  -- The join condition is type checked, joining Article.id' with
  -- User.id' is a type error.
  articles <- allByTitle `innerJoinOn` authorId -< User.id' users
  id -< (users, articles)

runById :: Transaction m => Id -> m (Maybe ArticleH)
runById = runQueryFirst . byId

insert :: Transaction m => Id -> User.Id -> Text -> Text -> m ()
insert i a t c =
  runInsert table psn
  where
    psn :: To Maybe (To Column Article)
    psn = Article
      { id'      = Just $ constant i
      , authorId = Just $ constant a
      , title    = Just $ constant t
      , content  = Just $ constant c
      }

update :: Transaction m => Id -> Text -> Text -> m Bool
update i t c = (> 0) <$> runUpdate table upd condition
  where
    upd :: To Column Article -> To Maybe (To Column Article)
    upd p = p
      { id'      = Just $ id' p
      , authorId = Just $ authorId p
      , title    = Just $ constant t
      , content  = Just $ constant c
      }
    condition :: To Column Article -> Column Bool
    condition p = id' p .== constant i

updateEasy :: Transaction m => Id -> Text -> Text -> m Bool
updateEasy i t c = (> 0) <$> runUpdateEasy table upd condition
  where
    upd :: To Column Article -> To Column Article
    upd p = p
      { id'      = id' p
      , authorId = authorId p
      , title    = constant t
      , content  = constant c
      }
    condition :: To Column Article -> Column Bool
    condition p = id' p .== constant i
