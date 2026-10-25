{-|
<<models/csg.glb>>
-}
module CsgExample
( csgExample
) where 

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Colour as Colour
import Waterfall.Booleans ( union, intersection, difference )
import Linear (V3 (..), unit, _x, _y )
import Data.Function ((&))

csgExample :: Solids.Solid
csgExample = let 
    sphere = Solids.colourSolid Colour.aqua Solids.unitSphere
    cube = Solids.colourSolid Colour.red 
          $ Transforms.uScale 1.5 Solids.centeredCube
    cylinder = Solids.centeredCylinder
         & Transforms.scale (V3 0.55 0.55 4) 
         & Solids.colourSolid Colour.lime
    cylinderA = Transforms.rotate (unit _x) (pi/2) cylinder
    cylinderB = Transforms.rotate (unit _y) (pi/2) cylinder
  in (cube `intersection` sphere) `difference` (cylinder `union` cylinderA `union` cylinderB)