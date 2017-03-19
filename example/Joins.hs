{-# LANGUAGE
    Arrows
  , FlexibleInstances
  , MultiParamTypeClasses
  , NoImplicitPrelude
  , NoMonomorphismRestriction
  , ScopedTypeVariables
  , TypeFamilies
  , UndecidableInstances
  #-}
module Joins
  ( articlesByUserName
  , usersWithLastArticle
  ) where

-- Per convention we use 'id' and '(.)' 'from Control.Category' If you
-- don't want this, use 'returnA' and '<<<' respectively instead.

import Control.Category

import Girella

import Article (Article)
import qualified Article
import User (User)
import qualified User

articlesByUserName :: Query (MapTo Column (User, Article))
articlesByUserName = proc () -> do
  users <- User.allByName -< ()
  -- The join condition is type checked, joining Article.id' with
  -- User.id' is a type error.
  articles <- Article.allByTitle `innerJoinOn` Article.authorId -< User.id' users
  id -< (users, articles)

usersWithLastArticle :: Query (To Column User, To Column (To Nullable Article))
usersWithLastArticle =
  leftJoin
    User.allByName
    Article.allByTitle
    (\(u, a) -> User.id' u .== Article.authorId a)
