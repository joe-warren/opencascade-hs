{-|
<<models/revolution.glb>>
-}
module RevolutionExample 
( revolutionExample 
) where

import Waterfall.Solids (Solid)
import qualified Waterfall.TwoD.Path2D as Path2D
import Waterfall.Revolution (revolution)
import Linear (V2 (..))

revolutionExample :: Solid
revolutionExample = 
    revolution $ 
        Path2D.pathFrom (V2 0 0)
            [ Path2D.lineTo (V2 1 0)
            , Path2D.lineRelative (V2 0.1 0.16)
            , Path2D.lineTo (V2 1 0.2)
            , Path2D.arcRelative Path2D.Clockwise 0.1 (V2 0 0.2)
            , Path2D.bezierRelative (V2 (-0.6) 0.0) (V2 (-0.8) 2.2) (V2 (-0.8) 2.6)
            , Path2D.lineTo (V2 0.5 3.0)
            , Path2D.lineRelative (V2 0.1 0.16)
            , Path2D.lineRelative (V2 (-0.2) 0.04)
            , Path2D.lineTo (V2 0.1 3.2)
            , Path2D.arcViaRelative (V2 0.5 0.6) (V2 (-0.1) 1.2)
            ]

