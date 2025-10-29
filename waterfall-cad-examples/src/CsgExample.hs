{-|
<<models/csg.glb>>
-}
module CsgExample
( csgExample
) where 

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Transforms as Transforms
import Waterfall.Booleans ( union, intersection, difference )
import Linear (V3 (..), (^*), unit, _x, _y )
import Data.Function ((&))

csgExample :: Solids.Solid
csgExample = let 
    sphere = Solids.unitSphere
    cube = Transforms.uScale 1.5 Solids.centeredCube
    cylinder = Solids.centeredCylinder
         & Transforms.scale (V3 0.55 0.55 4) 
    cylinderA = Transforms.rotate (unit _x) (pi/2) cylinder
    cylinderB = Transforms.rotate (unit _y) (pi/2) cylinder
  in (cube `intersection` sphere) `difference` (cylinder `union` cylinderA `union` cylinderB)