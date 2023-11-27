module SweepExample 
( sweepExample 
) where

import Waterfall.Sweep (sweep)
import Waterfall.Solids (Solid)
import qualified Waterfall.Path as Path
import qualified Waterfall.TwoD.Path2D as Path2D
import qualified Waterfall.TwoD.Shape as Shape
import Linear

sweepExample :: Solid
sweepExample = 

    let sweepPath = Path.pathFrom zero 
            [ Path.bezierRelative (V3 0 0 0.5) (V3 0.5 0.5 0.5) (V3 0.5 0.5 1)
            , Path.bezierRelative (V3 0 0 0.5) (V3 (-0.5) (-0.5) 0.5) (V3 (-0.5) (-0.5) 1)
            , Path.arcViaRelative (V3 0 1 1) (V3 0 2 0)
            , Path.lineTo (V3 0 2 0) 
            ] 
        sweepProfile = Shape.fromPath $
                Path2D.repeatLooping $
                Path2D.bezier (0.25 *^ unit _x) (0.5 *^ unit _x) (0.5 *^ angle (pi/6)) (0.25 *^ angle (pi/6))
    in sweep sweepPath sweepProfile
    
            {--Shape.fromPath $ (
                    Path2D.arcVia (V2 0 0.5) (V2 0.5 0) (V2 0 (-0.5))
                    <> Path2D.arcVia (V2 0 (-0.5)) (V2 (-0.5) 0 ) (V2 0 0.5)
                )--}