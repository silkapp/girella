{-# LANGUAGE TypeFamilies #-}
module Girella.To
  ( To
  , MapTo
  ) where

-- | Allows you to write To Column Person instead of Person (Column a) (Column b) [..]
type family To (a :: * -> *) (b :: *) :: *

-- These instances can be used if you want to use plain tuples instead of specific data types for queries. Not recommended.
type instance To x (a,b) = (x a,x b)
type instance To x (a,b,c) = (x a,x b,x c)
type instance To x (a,b,c,d) = (x a,x b,x c,x d)
type instance To x (a,b,c,d,e) = (x a,x b,x c,x d,x e)
type instance To x (a,b,c,d,e,f) = (x a,x b,x c,x d,x e,x f)
type instance To x (a,b,c,d,e,f,g) = (x a,x b,x c,x d,x e,x f,x g)
type instance To x (a,b,c,d,e,f,g,h) = (x a,x b,x c,x d,x e,x f,x g,x h)
type instance To x (a,b,c,d,e,f,g,h,i) = (x a,x b,x c,x d,x e,x f,x g,x h,x i)
type instance To x (a,b,c,d,e,f,g,h,i,j) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j)
type instance To x (a,b,c,d,e,f,g,h,i,j,k) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k)
type instance To x (a,b,c,d,e,f,g,h,i,j,k,l) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k,x l)
type instance To x (a,b,c,d,e,f,g,h,i,j,k,l,m) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k,x l,x m)
type instance To x (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k,x l,x m,x n)
type instance To x (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k,x l,x m,x n,x o)
type instance To x (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k,x l,x m,x n,x o,x p)
type instance To x (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = (x a,x b,x c,x d,x e,x f,x g,x h,x i,x j,x k,x l,x m,x n,x o,x p,x q)

-- MapTo x (y,z) is just a convenience for (To x y, To x z) and isn't that useful. Might be removed.
type family MapTo (t :: * -> *) a :: *

type instance MapTo x (a,b) = (To x a,To x b)
type instance MapTo x (a,b,c) = (To x a,To x b,To x c)
type instance MapTo x (a,b,c,d) = (To x a,To x b,To x c,To x d)
type instance MapTo x (a,b,c,d,e) = (To x a,To x b,To x c,To x d, To x e)
type instance MapTo x (a,b,c,d,e,f) = (To x a,To x b,To x c,To x d, To x e, To x f)
type instance MapTo x (a,b,c,d,e,f,g) = (To x a,To x b,To x c,To x d, To x e, To x f, To x g)
