module Silk.Opaleye.TH.Util
  ( getConNameTy
  , ambiguateName
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Silk.Opaleye.Misc (trd3)

ambiguateName :: Name -> Name
ambiguateName (Name occ _) = Name occ NameS

getConName :: Con -> Either String Name
getConName c = case c of
  NormalC conName _ -> Right conName
  RecC conName _ -> Right conName
  _ -> Left "Only normal and record constructors are allowed"

getConTy :: Con -> Either String [Type]
getConTy c = case c of
  NormalC _ t -> Right $ map snd t
  RecC _ t    -> Right $ map trd3 t
  _           -> Left "Only normal and record constructors are allowed"

getConNameTy :: Con -> Either String (Name, [Type])
getConNameTy c = do
  nm <- getConName c
  t  <- getConTy c
  return (nm,t)
