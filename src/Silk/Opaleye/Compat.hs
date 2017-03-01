{-# LANGUAGE CPP #-}
module Silk.Opaleye.Compat
  ( unsafeCoerceColumn
  , QueryRunnerColumnDefault (..)
  , PGOrd
  , PGIntegral
  , PGString
  -- * template-haskell
  , VarBangType
  , equalP_
  , classP_
  , instanceD_
  , newtypeDView
  , dataDView
  -- * base: call stacks
  , SrcLoc
  , prettySrcLoc
  ) where

import Language.Haskell.TH (Con, Cxt, Dec (DataD, InstanceD, NewtypeD), Name, Pred, TyVarBndr, Type (ConT, EqualityT, AppT))

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Syntax (VarBangType)
#else
import Language.Haskell.TH.Syntax (VarStrictType)
#endif

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (SrcLoc, prettySrcLoc)
#else
import GHC.SrcLoc (SrcLoc, showSrcLoc)
#endif

#if MIN_VERSION_template_haskell(2,10,0)
import Data.Foldable (foldl')
#endif

#if MIN_VERSION_opaleye(0,5,0)
import Opaleye.Internal.Column (PGIntegral, PGString)
#endif

#if MIN_VERSION_opaleye(0,4,0)
import Opaleye.Column (unsafeCoerceColumn)
import Opaleye.Order (PGOrd)
import Opaleye.RunQuery (QueryRunnerColumnDefault (..))
#else
import Opaleye.Column (Column, Nullable, unsafeCoerce)
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault (..))
import Opaleye.PGTypes
#endif

#if !MIN_VERSION_opaleye(0,4,0)
unsafeCoerceColumn :: Column a -> Column b
unsafeCoerceColumn = unsafeCoerce
#endif

#if !MIN_VERSION_opaleye(0,4,0)
class PGOrd a
instance PGOrd PGBool
instance PGOrd PGCitext
instance PGOrd PGDate
instance PGOrd PGFloat4
instance PGOrd PGFloat8
instance PGOrd PGInt2
instance PGOrd PGInt4
instance PGOrd PGInt8
instance PGOrd PGNumeric
instance PGOrd PGText
instance PGOrd PGTime
instance PGOrd PGTimestamp
instance PGOrd PGTimestamptz
instance PGOrd PGUuid
instance PGOrd a => PGOrd (Nullable a)
#endif

#if !MIN_VERSION_opaleye(0,5,0)
class PGIntegral a
instance PGIntegral PGInt2
instance PGIntegral PGInt4
instance PGIntegral PGInt8

class PGString a
instance PGString PGText
instance PGString PGCitext
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

instanceD_ :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
instanceD_ =
  InstanceD Nothing
#else
instanceD_ =
  InstanceD
#endif

#if !MIN_VERSION_template_haskell(2,11,0)
type VarBangType = VarStrictType
#endif

newtypeDView :: Dec -> Maybe (Name, Con, [TyVarBndr])
#if MIN_VERSION_template_haskell(2,11,0)
newtypeDView (NewtypeD _preds tyName tys _mkind con _preds') =
  Just (tyName, con, tys)
#else
newtypeDView (NewtypeD _preds tyName tys con _names) =
  Just $ (tyName, con, tys)
#endif
newtypeDView _ = Nothing

dataDView :: Dec -> Maybe (Cxt, Name, [TyVarBndr], [Con])
#if MIN_VERSION_template_haskell(2,11,0)
dataDView (DataD ctx name tys _mkind cons _ctx) =
  Just (ctx, name, tys, cons)
#else
dataDView (DataD ctx name tys cons _names) =
  Just (ctx, name, tys, cons)
#endif
dataDView _ = Nothing

#if !MIN_VERSION_base(4,9,0)
prettySrcLoc :: SrcLoc -> String
prettySrcLoc = showSrcLoc
#endif
