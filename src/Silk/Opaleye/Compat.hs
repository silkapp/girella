{-# LANGUAGE CPP #-}
module Silk.Opaleye.Compat
  ( unsafeCoerceColumn
  , QueryRunnerColumnDefault (..)
  , PGOrd
  ) where

#if MIN_VERSION_opaleye(0,4,0)
import Opaleye.Column (unsafeCoerceColumn)
import Opaleye.Order (PGOrd)
import Opaleye.RunQuery (QueryRunnerColumnDefault (..))
#else
import Opaleye.Column (Column, unsafeCoerce)
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault (..))
#endif

#if !MIN_VERSION_opaleye(0,4,0)
unsafeCoerceColumn :: Column a -> Column b
unsafeCoerceColumn = unsafeCoerce
#endif

#if !MIN_VERSION_opaleye(0,4,0)
class PGOrd a
#endif
