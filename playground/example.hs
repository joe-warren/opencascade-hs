import qualified Waterfall.Solids as Solids
import qualified Waterfall.Transforms as Transforms
import Waterfall.Booleans ( union, intersection, difference )
import Linear (V3 (..), (^*), unit, _x, _y )
import Data.Function ((&))

sphere = Solids.unitSphere

cube = Transforms.uScale 1.5 Solids.centeredCube

cylinder 
    = Solids.centeredCylinder
        & Transforms.scale (V3 0.55 0.55 4)

cylinderA = Transforms.rotate (unit _x) (pi/2) cylinder

cylinderB = Transforms.rotate (unit _y) (pi/2) cylinder

cross =  cylinder `union` cylinderA `union` cylinderB

dieShape = cube `intersection` sphere

csgExample = dieShape `difference` cross 