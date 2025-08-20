{-|
<<models/2d-booleans.glb>>
-}
module TwoDBooleansExample 
( twoDBooleansExample
) where

import qualified Waterfall.TwoD.Shape as Shape
import Waterfall.TwoD.Booleans (union2D, intersection2D, difference2D)
import Waterfall.TwoD.Booleans.Operators ((~/\~), (~\/~), (~-~))
import Waterfall.TwoD.Transforms (translate2D)
import Waterfall.Solids (prism, Solid)
import Linear (V2 (..))

-- | Example showing 2D boolean operations
twoDBooleansExample :: Solid
twoDBooleansExample = 
    let 
        -- Create basic shapes
        circle = Shape.unitCircle
        square = Shape.centeredSquare
        
        -- Translate square to create overlap
        offsetSquare = translate2D (V2 0.5 0.5) square
        
        -- Boolean operations
        unionShape = circle `union2D` offsetSquare
        intersectionShape = circle `intersection2D` offsetSquare  
        differenceShape = circle `difference2D` offsetSquare
        
        -- Using infix operators
        complexShape = (circle ~\/~ offsetSquare) ~-~ (circle ~/\~ offsetSquare)
        
        -- Extrude the final shape to 3D
    in prism 0.2 complexShape
