module PrismExample
( prismExample
) where 

import qualified Waterfall.Solids as Solids
import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.TwoD.Path2D as Path2D
import Linear (V2 (..))

prismExample :: Solids.Solid
prismExample = Solids.prism 1 . Shape.fromPath $
    Path2D.pathFrom (V2 (-1) (-1) :: V2 Double) 
        [ Path2D.arcViaTo (V2 (-1.5) 0) (V2 (-1) 1)
        , Path2D.lineTo (V2 1 1)
        , Path2D.bezierTo (V2 1.5 1) (V2 1.5 (-1)) (V2 1 (-1))
        , Path2D.lineTo (V2 (-1) (-1))
        ]