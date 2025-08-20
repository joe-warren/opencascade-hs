module Waterfall.TwoD.Booleans.Operators
( (~/\~)  -- intersection
, (~\/~)  -- union  
, (~-~)   -- difference
) where

import Waterfall.TwoD.Booleans
import Waterfall.TwoD.Internal.Shape (Shape)

-- | Infix version of `intersection2D`
(~/\~) :: Shape -> Shape -> Shape
(~/\~) = intersection2D

-- | Infix version of `union2D`
(~\/~) :: Shape -> Shape -> Shape
(~\/~) = union2D

-- | Infix version of `difference2D`
(~-~) :: Shape -> Shape -> Shape
(~-~) = difference2D
