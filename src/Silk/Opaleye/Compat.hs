{-# LANGUAGE
    CPP
  , FlexibleInstances
  , TypeSynonymInstances
  #-}
module Silk.Opaleye.Compat
  ( equalP_
  , classP_
  ) where

import Language.Haskell.TH

#if MIN_VERSION_template_haskell(2,10,0)
import Data.Foldable (foldl')
#endif

equalP_ :: Type -> Type -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
equalP_ t1 t2 = EqualityT `AppT` t1 `AppT` t2
#else
equalP_ = EqualP
#endif

classP_ :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP_ = foldl' AppT . ConT
#else
classP_ = ClassP
#endif
