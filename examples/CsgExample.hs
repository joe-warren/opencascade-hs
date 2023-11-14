module CsgExample
( csgExample
) where 

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Transforms as Transforms
import Waterfall.Booleans.Operators 
import Linear.V3 as V3


csgExample :: Solids.Solid
csgExample = let 
    sphere = Solids.unitSphere
    cube = Transforms.uScale 1.5 Solids.centeredCube
    cylinder = Transforms.scale (V3.V3 0.5 0.5 2) Solids.unitCylinder
    cylinderA = Transforms.rotate (V3.V3 1 0 0) (pi/2) cylinder
    cylinderB = Transforms.rotate (V3.V3 0 1 0) (pi/2) cylinder
  in (cube ~/\~ sphere) ~-~ (cylinder ~\/~ cylinderA ~\/~ cylinderB)