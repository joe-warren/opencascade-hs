module Waterfall.Booleans.Operators
((~/\~), (~\/~), (~-~)
)where 

import Waterfall.Booleans
import Waterfall.Solids (Solid)

-- | Infix version of `intersection`
(~/\~) :: Solid -> Solid -> Solid
(~/\~) = intersection

-- | Infix version of `union`
(~\/~) :: Solid -> Solid -> Solid
(~\/~) = union

-- | Infix version of `difference`
(~-~) :: Solid -> Solid -> Solid
(~-~) = difference
