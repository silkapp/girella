{-# LANGUAGE CPP #-}
module Silk.Opaleye.Compat
  ( unsafeCoerceColumn
  , QueryRunnerColumnDefault (..)
  , PGOrd
  , equalP_
  , classP_
  ) where

import Language.Haskell.TH

#if MIN_VERSION_template_haskell(2,10,0)
import Data.Foldable (foldl')
#endif

#if MIN_VERSION_opaleye(0,4,0)
import Opaleye.Column (unsafeCoerceColumn)
import Opaleye.Order (PGOrd)
import Opaleye.RunQuery (QueryRunnerColumnDefault (..))
#else
import Opaleye.Column (Column, unsafeCoerce)
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault (..))
import Opaleye.PGTypes
#endif

#if !MIN_VERSION_opaleye(0,4,0)
unsafeCoerceColumn :: Column a -> Column b
unsafeCoerceColumn = unsafeCoerce
#endif

#if !MIN_VERSION_opaleye(0,4,0)
class PGOrd a
instance PGOrd PGInt4
instance PGOrd PGInt8
instance PGOrd PGTimestamptz
instance PGOrd PGText
instance PGOrd PGCitext
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
