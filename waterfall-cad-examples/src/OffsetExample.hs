module OffsetExample 
( offsetExample 
) where

import qualified Waterfall.Solids as Solids
import Waterfall.Transforms (translate, rotate, scale)
import Waterfall.Offset (offset)
import Linear ( V3(V3), unit, _x, _y, _z, (^*))

offsetExample :: Solids.Solid
offsetExample = 
  let beam axis = rotate axis (pi/2) (scale (V3 2 2 4) Solids.centeredCube)
      cross = foldMap beam [unit _x,  unit _y, unit _z]
      offsetCross amount = offset amount cross
      offsetCrosses = offsetCross <$> [- 0.5, - 0.25, 0, 0.25, 0.5] 
      position = (unit _x ^* 5 ^*) <$> [0..]  
  in mconcat $ zipWith translate position offsetCrosses 