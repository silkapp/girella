module Silk.Opaleye.Range (Range (..)) where

data Range = Range
  { offset :: Int
  , limit  :: Int
  } deriving (Show, Eq)
