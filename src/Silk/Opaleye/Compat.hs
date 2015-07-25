{-# LANGUAGE CPP #-}
module Silk.Opaleye.Compat
  ( unsafeCoerceColumn
  , QueryRunnerColumnDefault (..)
  ) where

#if MIN_VERSION_opaleye(0,4,0)
import Opaleye.Column (unsafeCoerceColumn)
import Opaleye.RunQuery (QueryRunnerColumnDefault (..))
#else
import Opaleye.Column (Column, unsafeCoerce)
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault (..))
#endif

#if !MIN_VERSION_opaleye(0,4,0)
unsafeCoerceColumn :: Column a -> Column b
unsafeCoerceColumn = C.unsafeCoerce
#endif
