{-|
<<models/2d-booleans.glb>>
-}
module TwoDBooleansExample 
( twoDBooleansExample
) where

import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Booleans as Booleans
import Waterfall.TwoD.Transforms (translate2D)
import Waterfall.Solids (prism, Solid)
import Linear (V2 (..))

twoDBooleansExample :: Solid
twoDBooleansExample = 
    let offsetSquare = translate2D (V2 0.5 0.5) Shape.centeredSquare
        complexShape = Booleans.difference 
            (Booleans.union Shape.unitCircle offsetSquare)
            (Booleans.intersection Shape.unitCircle offsetSquare)
    in prism 0.2 complexShape
