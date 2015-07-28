{-# LANGUAGE
    CPP
  , FlexibleInstances
  , TypeSynonymInstances
  #-}
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
