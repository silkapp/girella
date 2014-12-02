-- Module used from code that only runs predefined db actions.
module Silk.Opaleye.Run
  ( MonadPool
  , runTransaction
  ) where

import Silk.Opaleye
