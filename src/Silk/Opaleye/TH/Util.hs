module Silk.Opaleye.TH.Util
  ( getConNameTy
  , ty
  , simpleName
  , ambiguateName
  , multiAppE
  ) where

import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

multiAppE :: Exp -> [Exp] -> Exp
multiAppE x ys = foldl' (\a b-> a `AppE` b) x ys

simpleName :: String -> Name
simpleName = mkName

ambiguateName :: Name -> Name
ambiguateName (Name occ _) = Name occ NameS

ty :: String -> Type
ty = ConT . simpleName

getConName :: Con -> Either String Name
getConName c = case c of
  NormalC conName _ -> Right conName
  RecC conName _ -> Right conName
  _ -> Left "Only normal and record constructors are allowed"

getConTy :: Con -> Either String [Type]
getConTy c = case c of
  NormalC _ t -> Right $ map (\(_,b) -> b) t
  RecC _ t -> Right $ map (\(_,_,b) -> b) t
  _ -> Left "Only normal and record constructors are allowed"

getConNameTy :: Con -> Either String (Name, [Type])
getConNameTy c = do
  nm <- getConName c
  t <- getConTy c
  return $ (nm,t)
