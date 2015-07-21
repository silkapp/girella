{-# LANGUAGE
    DefaultSignatures
  , TypeFamilies
  #-}
module Silk.Opaleye.Conv (Conv (..)) where

-- | Convert an opaleye result to a separate type.
--
-- Also has a default implementation for the identity conversion so
-- both domain and database functions can use query functions in
-- the same way.
class Conv d where
  type OpaRep d :: *
  type OpaRep d = d
  conv :: OpaRep d -> d
  default conv :: (OpaRep d ~ d) => OpaRep d -> d
  conv = id

instance Conv a => Conv [a] where
  type OpaRep [a] = [OpaRep a]
  conv = fmap conv
