{-# LANGUAGE NoImplicitPrelude #-}
module Silk.Opaleye.TH.Column
  ( -- * TH end points
    mkId
  , makeColumnInstances
  , makeColumnInstancesWithoutConv
    -- * TH dependencies defined here
  , fromFieldTotal
  , fromFieldAux
    -- * Re-exported TH dependencies
  , Typeable
  , Default (def)
  , ShowConstant (..)
  , FromField (fromField)
  , QueryRunnerColumnDefault (..)
  , Nullable
  , Column
  , fieldQueryRunnerColumn
  , unsafeCoerceColumn
  , Conv
  ) where

import Prelude.Compat

import Control.Monad ((<=<))
import Data.Data (Typeable)
import Data.Profunctor.Product.Default (Default (def))
import Data.String.Conversions (StrictByteString, cs)
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (..), ResultError (..),
                                             returnError)
import Language.Haskell.TH
import Opaleye.Column (Column, Nullable)
import Opaleye.RunQuery (fieldQueryRunnerColumn)

import Silk.Opaleye.Compat (QueryRunnerColumnDefault (..), classP_, equalP_, unsafeCoerceColumn)
import Silk.Opaleye.Conv (Conv)
import Silk.Opaleye.ShowConstant (ShowConstant (..))


-- | Given a @newtype@ declaration with no type parameters and a single
-- record field destructor of the form
--
-- > newtype <TypeName> = <TypeConstructor> { <destructor> :: <InnerType> }
--
-- we generate all the instances a column type needs in a more magic way
-- than calling 'makeColumnInstances' manually.
mkId :: Name -> Q [Dec]
mkId = return . either error id <=< f <=< reify
  where
    f :: Info -> Q (Either String [Dec])
    f i = case i of
      TyConI (NewtypeD _ctx tyName _tyVars@[] con _names) ->
        case con of
          RecC conName [(desName , _ , innerTy)] -> Right <$> g tyName conName innerTy desName
          _ -> return $ Left "Must be a newtype without type parameters and a single destructor record field"
      TyConI NewtypeD{} -> return $ Left "Type variables aren't allowed"
      _                 -> return $ Left "Must be a newtype"

    g :: Name -> Name -> Type -> Name -> Q [Dec]
    g tyName conName innerTy desName = makeColumnInstancesInternal tyName innerTy desName (Left conName) True

makeColumnInstances :: Name -> Name -> Name -> Name -> Q [Dec]
makeColumnInstances tyName innerTyName toDb fromDb = makeColumnInstancesInternal tyName (ConT innerTyName) toDb (Right fromDb) True

makeColumnInstancesWithoutConv :: Name -> Name -> Name -> Name -> Q [Dec]
makeColumnInstancesWithoutConv tyName innerTyName toDb fromDb = makeColumnInstancesInternal tyName (ConT innerTyName) toDb (Right fromDb) False

makeColumnInstancesInternal :: Name -> Type -> Name -> Either Name Name -> Bool -> Q [Dec]
makeColumnInstancesInternal tyName innerTy toDb fromDb convInstance = do
  tvars <- getTyVars tyName
  let predCond = map (classP_ (mkName "Typeable") . (:[])) tvars
  let outterTy = foldl AppT (ConT tyName) tvars
  return $ map ($ (predCond, outterTy)) $ [fromFld, showConst, queryRunnerColumn] ++ if convInstance then [conv] else []
  where
    fromFld (predCond, outterTy)
      = InstanceD
          predCond
          (ConT (mkName "FromField") `AppT` outterTy)
          [ FunD (mkName "fromField")
            [ Clause
                []
                (NormalB $ case fromDb of
                   Left  l -> VarE (mkName "fromFieldTotal") `AppE` ConE l
                   Right r -> VarE (mkName "fromFieldAux"  ) `AppE` VarE r
                )
                []
            ]
          ]
    showConst (_, outterTy)
      = InstanceD
          []
          (ConT (mkName "ShowConstant") `AppT` outterTy)
          [ TySynInstD (mkName "PGRep") (TySynEqn [outterTy] (ConT (mkName "PGRep") `AppT` innerTy))
          , FunD (mkName "constant")
            [ Clause []
              (NormalB $ InfixE
               (Just (VarE (mkName "unsafeCoerceColumn")))
               (VarE (mkName "."))
               (Just (InfixE (Just (VarE (mkName "constant"))) (VarE (mkName ".")) (Just (VarE toDb))))
              )
              []
            ]
          ]
    queryRunnerColumn (predCond, outterTy)
      = InstanceD
          (equalP_ (ConT (mkName "PGRep") `AppT` outterTy) (VarT $ mkName "a") : predCond)
          (ConT (mkName "QueryRunnerColumnDefault") `AppT` outterTy `AppT` outterTy)
          [ FunD
            (mkName "queryRunnerColumnDefault")
            [ Clause [] (NormalB $ VarE $ mkName "fieldQueryRunnerColumn") [] ]
          ]
    conv (_, outerTy) = InstanceD [] (ConT (mkName "Conv") `AppT` outerTy) []

getTyVars :: Name -> Q [Type]
getTyVars n = do
  info <- reify n
  return . map varToType $ case info of
    TyConI (DataD    _ _ tvars _ _) -> tvars
    TyConI (NewtypeD _ _ tvars _ _) -> tvars
    TyConI (TySynD   _   tvars _  ) -> tvars
    _                               -> []
  where
    varToType (PlainTV nv)    = VarT nv
    varToType (KindedTV nv _) = VarT nv

fromFieldTotal :: (FromField a, Typeable b) => (a -> b) -> Field -> Maybe StrictByteString -> Conversion b
fromFieldTotal fromDb f mdata = fromDb <$> fromField f mdata

fromFieldAux :: (FromField a, Typeable b) => (a -> Maybe b) -> Field -> Maybe StrictByteString -> Conversion b
fromFieldAux fromDb f mdata = case mdata of
  Just dat -> maybe (returnError ConversionFailed f (cs dat)) return . fromDb =<< fromField f mdata
  Nothing  -> returnError UnexpectedNull f ""
