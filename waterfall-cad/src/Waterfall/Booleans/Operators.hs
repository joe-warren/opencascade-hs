module Waterfall.Booleans.Operators
(
    (~/\~), (~\/~), (~-~)

)where 

import Waterfall.Booleans
import Waterfall.Solids (Solid)

(~/\~) :: Solid -> Solid -> Solid
(~/\~) = intersection


(~\/~) :: Solid -> Solid -> Solid
(~\/~) = union

(~-~) :: Solid -> Solid -> Solid
(~-~) = difference
