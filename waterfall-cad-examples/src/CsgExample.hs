{-|
<<models/csg.glb>>
-}
module CsgExample
( csgExample
) where 

import qualified Waterfall.Solid as Solid
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Colour as Colour
import Waterfall.Booleans ( union, intersection, difference )
import Linear (V3 (..), unit, _x, _y )
import Data.Function ((&))

csgExample :: Solid.Solid
csgExample = let 
    sphere = Solid.colourSolid Colour.aqua Solid.unitSphere
    cube = Solid.colourSolid Colour.red 
          $ Transforms.uScale 1.5 Solid.centeredCube
    cylinder = Solid.centeredCylinder
         & Transforms.scale (V3 0.55 0.55 4) 
         & Solid.colourSolid Colour.lime
    cylinderA = Transforms.rotate (unit _x) (pi/2) cylinder
    cylinderB = Transforms.rotate (unit _y) (pi/2) cylinder
  in (cube `intersection` sphere) `difference` (cylinder `union` cylinderA `union` cylinderB)