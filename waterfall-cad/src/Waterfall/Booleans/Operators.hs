module Waterfall.Booleans.Operators
((~/\~), (~\/~), (~-~)
)where 

import Waterfall.Booleans (Boolean(..))

-- | Infix version of `intersection` (works for both 2D and 3D objects)
(~/\~) :: Boolean a => a -> a -> a
(~/\~) = intersection

-- | Infix version of `union` (works for both 2D and 3D objects)
(~\/~) :: Boolean a => a -> a -> a
(~\/~) = union

-- | Infix version of `difference` (works for both 2D and 3D objects)
(~-~) :: Boolean a => a -> a -> a
(~-~) = difference
