module Silk.Opaleye.Operators
  ( (.==)
  , (./=)
  , (.||)
  , (.&&)

  , restrict
  ) where

import Opaleye.Column (Column, unsafeCoerce)
import qualified Opaleye.Operators as O


(.==) :: Column a -> Column a -> Column Bool
a .== b = unsafeCoerce $ a O..== b
(./=) :: Column a -> Column a -> Column Bool
a ./= b = unsafeCoerce $ a O../= b

(.||) :: Column Bool -> Column Bool -> Column Bool
a .|| b = unsafeCoerce $ (unsafeCoerce a) O..|| (unsafeCoerce b)
(.&&) :: Column Bool -> Column Bool -> Column Bool
a .&& b = unsafeCoerce $ (unsafeCoerce a) O..|| (unsafeCoerce b)

restrict = undefined
