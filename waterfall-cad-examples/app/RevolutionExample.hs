module RevolutionExample 
( revolutionExample 
) where

import Waterfall.Solids (Solid)
import qualified Waterfall.TwoD.Path2D as Path2D
import Waterfall.Revolution (revolution)
import Linear

revolutionExample :: Solid
revolutionExample = 
    revolution $ 
        Path2D.pathFrom (V2 0 0)
            [ Path2D.lineTo (V2 1 0)
            , Path2D.lineTo (V2 1.1 0.16)
            , Path2D.lineTo (V2 1 0.2)
            , Path2D.arcTo Path2D.Clockwise 0.1 (V2 1 0.4)
            , Path2D.bezierTo (V2 0.4 0.4) (V2 0.2 2.6) (V2 0.2 3.0)
            , Path2D.lineTo (V2 0.5 3.0)
            , Path2D.lineTo (V2 0.6 3.16)
            , Path2D.lineTo (V2 0.5 3.2)
            , Path2D.lineTo (V2 0.1 3.2)
            , Path2D.arcViaTo (V2 0.6 3.8) (V2 0 4.4)
            ]

