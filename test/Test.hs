{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module Test where

import Control.Monad.Reader
import Silk.Opaleye
import Silk.Opaleye.TH

connectInfo :: ConnectInfo
connectInfo =
  ConnectInfo
    { connectHost     = "localhost"
    , connectPort     = 5432
    , connectUser     = "silk"
    , connectPassword = "s1lK5iLk!"
    , connectDatabase = "silk_test"
    }

newtype Id = Id { unId :: Int }
  deriving (Show, Typeable)

mkId ''Id

makeTypes [d|
    data Test = Test
      { id'  :: Id
      , user :: Nullable Int
      , name :: String
      } deriving Show
  |]

makeAdaptorAndInstance "pTest" ''TestP

makeTable "insert_test" 'pTest ''TestP

instance Conv TestH

queryAll :: Query (To Column Test)
queryAll = queryTable table

updateTest :: Transaction m => Id -> Maybe Int -> String -> m Bool
updateTest eid mi s = fmap (> 0) $ runUpdateConst table upd (\u -> id' u .== constant eid)
  where
    upd = emptyUpdate
        { user = Just $ maybeToNullable mi }

newTest :: Transaction m => Maybe Int -> String -> m [TestH]
newTest mi s
  = runInsertReturning table emb id
  where
    emb :: To Maybe (To Column Test)
    emb = Test
        { id'          = Nothing
        , user  = Just $ maybeToNullable mi
        , name  = Just $ constant s
        }

main :: IO ()
main =
  do
     opacfg <- defaultConfig connectInfo
     runReaderT doit opacfg
  where
    doit = do
      [x] <- runTransaction $ newTest (Just 1) "hello"
      _   <- runTransaction $ updateTest (id' x) (fmap (+2) $ user x) "zello"
      return ()
