module Girella.Table
  ( module Opaleye.Table
  , optionalColumn
  ) where

import Opaleye.Column (Column)
import Opaleye.Table hiding (optional)
import qualified Opaleye.Table as T (optional)

optionalColumn :: String -> TableProperties (Maybe (Column a)) (Column a)
optionalColumn = T.optional
