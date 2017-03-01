{-# LANGUAGE
    NoImplicitPrelude
  , TemplateHaskell
  , ViewPatterns
  #-}
module Silk.Opaleye.TH.Column
  ( -- * TH end points
    mkId
  , makeColumnInstances
  , makeColumnInstancesWithoutConv

  , fromFieldAux
  , fromFieldTotal
  ) where

import Prelude.Compat

import Control.Monad ((<=<))
import Data.Data (Typeable)
import Data.String.Conversions (StrictByteString, cs)
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (..), ResultError (..), returnError)
import Language.Haskell.TH
import Opaleye.RunQuery (fieldQueryRunnerColumn)

import Silk.Opaleye.Compat (QueryRunnerColumnDefault (..), classP_, dataDView, equalP_, instanceD_, newtypeDView, unsafeCoerceColumn)
import Silk.Opaleye.Conv (Conv)
import Silk.Opaleye.ShowConstant (PGRep, ShowConstant (..))


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
      TyConI (newtypeDView -> Just (tyName, con, _)) ->
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
  let predCond = map (classP_ (''Typeable) . (:[])) tvars
  let outterTy = foldl AppT (ConT tyName) tvars
  return $ map ($ (predCond, outterTy)) $ [fromFld, pgRep, showConst, queryRunnerColumn] ++ if convInstance then [conv] else []
  where
    fromFld :: ([Pred], Type) -> Dec
    fromFld (predCond, outterTy)
      = instanceD_
          predCond
          (ConT ''FromField `AppT` outterTy)
          [ FunD 'fromField
            [ Clause
                []
                (NormalB $ case fromDb of
                   Left  l -> VarE 'fromFieldTotal `AppE` ConE l
                   Right r -> VarE 'fromFieldAux   `AppE` VarE r
                )
                []
            ]
          ]
    pgRep :: ([Pred], Type) -> Dec
    pgRep (_, outterTy) = TySynInstD (''PGRep) (TySynEqn [outterTy] (ConT ''PGRep `AppT` innerTy))
    showConst :: ([Pred], Type) -> Dec
    showConst (_, outterTy)
      = instanceD_
          []
          (ConT ''ShowConstant `AppT` outterTy)
          [ FunD 'constant
            [ Clause []
              (NormalB $ InfixE
               (Just (VarE 'unsafeCoerceColumn))
               (VarE (mkName "."))
               (Just (InfixE (Just (VarE 'constant)) (VarE (mkName ".")) (Just (VarE toDb))))
              )
              []
            ]
          ]
    queryRunnerColumn :: ([Pred], Type) -> Dec
    queryRunnerColumn (predCond, outterTy)
      = instanceD_
          (equalP_ (ConT ''PGRep `AppT` outterTy) (VarT $ mkName "a") : predCond)
          (ConT ''QueryRunnerColumnDefault `AppT` outterTy `AppT` outterTy)
          [ FunD
            'queryRunnerColumnDefault
            [ Clause [] (NormalB $ VarE 'fieldQueryRunnerColumn) [] ]
          ]
    conv :: ([Pred], Type) -> Dec
    conv (_, outerTy) = instanceD_ [] (ConT ''Conv `AppT` outerTy) []

getTyVars :: Name -> Q [Type]
getTyVars n = do
  info <- reify n
  return . map varToType $ case info of
    TyConI (dataDView -> Just (_, _, tvars, _)) -> tvars
    TyConI (newtypeDView -> Just (_, _, tvars)) -> tvars
    TyConI (TySynD _ tvars _ ) -> tvars
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
