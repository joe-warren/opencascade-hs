module RevolutionExample 
( revolutionExample 
) where

import Waterfall.Solids (Solid)
import qualified Waterfall.TwoD.Path as Path
import Waterfall.Revolution (revolution)
import Linear

revolutionExample :: Solid
revolutionExample = 
    revolution $ 
        Path.pathFrom (V2 0 0)
            [ Path.lineTo (V2 1 0)
            , Path.lineTo (V2 1.1 0.16)
            , Path.lineTo (V2 1 0.2)
            , Path.arcTo Path.Clockwise 0.1 (V2 1 0.4)
            , Path.bezierTo (V2 0.4 0.4) (V2 0.2 2.6) (V2 0.2 3.0)
            , Path.lineTo (V2 0.5 3.0)
            , Path.lineTo (V2 0.6 3.16)
            , Path.lineTo (V2 0.5 3.2)
            , Path.lineTo (V2 0.1 3.2)
            , Path.arcViaTo (V2 0.6 3.8) (V2 0 4.4)
            ]

