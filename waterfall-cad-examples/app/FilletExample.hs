module FilletExample (
    filletExample
) where

import Waterfall.Solids( Solid, centeredCube )
import Waterfall.Fillet(roundFillet )

filletExample :: Solid
filletExample = roundFillet 0.1 centeredCube

