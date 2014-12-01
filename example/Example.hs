{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , TemplateHaskell
  , TypeFamilies
  #-}
module Example where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Data.UUID

import Silk.Opaleye hiding (name)
import Silk.Opaleye.TH

newtype Id = Id { unId :: UUID }
  deriving (Show, Typeable)

mkId ''Id

data Gender = Male | Female
  deriving (Show, Typeable)

genderToString :: Gender -> String
genderToString = \case
  Male   -> "male"
  Female -> "female"

stringToGender :: String -> Maybe Gender
stringToGender = \case
  "male"   -> Just Male
  "female" -> Just Female
  _        -> Nothing

makeColumnInstances ''Gender 'genderToString 'stringToGender

makeTypes [d|
    data Person = Person
      { id_    :: Id
      , name   :: String
      , age    :: Int
      , gender :: Nullable Gender
      } deriving Show
  |]

makeAdaptorAndInstance "pPerson" ''PersonP

instance Conv PersonH

makeTable_ "people" 'pPerson ''PersonP

queryAll :: Query (To Column Person)
queryAll = queryTable table

byId :: UUID -> Query (To Column Person)
byId i = where_ (\u -> id_ u .== constant (unsafeId i)) . queryAll

nameOrder :: Order (To Column Person)
nameOrder = asc (lower . arr name)

allByName :: Query (To Column Person)
allByName = orderBy nameOrder queryAll

-- Generally :: (Transaction m, Conv domain, OpaRep domain ~ PersonH) => UUID -> m (Maybe domain)
runById :: Transaction m => UUID -> m (Maybe PersonH)
runById = runQueryFirst . byId

insert :: Transaction m => String -> Int -> Maybe Gender -> m ()
insert n a mg = runInsert table psn
  where
    psn :: To Maybe (To Column Person)
    psn = Person
      { id_    = Nothing
      , name   = Just $ constant n
      , age    = Just $ constant a
      , gender = Just $ maybeToNullable mg
      }

-- Type sig can be generalized to Conv as above.
insertAndSelectAll :: Transaction m => String -> Int -> Maybe Gender -> m [PersonH]
insertAndSelectAll n a mg = do
  insert n a mg
  runQuery queryAll

-- Usually no point in defining this function by itself, but it could form a larger transaction.
runInsertAndSelectAll :: Database m => String -> Int -> Maybe Gender -> m [PersonH]
runInsertAndSelectAll n a mg = runTransaction $ insertAndSelectAll n a mg
