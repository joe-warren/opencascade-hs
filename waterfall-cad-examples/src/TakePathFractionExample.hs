{-|
<<models/takePathFraction.glb>>
-}
module TakePathFractionExample
( takePathFractionExample
) where

import Waterfall.Sweep (sweep)
import Waterfall.Solids (Solid)
import qualified Waterfall.Path as Path
import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import Linear (V3 (..), zero)

takePathFractionExample :: Solid
takePathFractionExample =
    let sweepPath = Path.pathFrom zero
            [ Path.bezierRelative (V3 0 0 0.5) (V3 0.5 0.5 0.5) (V3 0.5 0.5 1)
            , Path.bezierRelative (V3 0 0 0.5) (V3 (-0.5) (-0.5) 0.5) (V3 (-0.5) (-0.5) 1)
            , Path.arcViaRelative (V3 0 1 1) (V3 0 2 0)
            , Path.lineTo (V3 0 2 0)
            ]
        profile = TwoD.Transforms.uScale2D 0.2 Shape.unitCircle
        fractions = [0.1, 0.2 .. 1.0]
    in mconcat
        [ Transforms.translate (V3 (f * 10) 0 0) . sweep (Path.takePathFraction3D f sweepPath) $ profile
        | f <- fractions
        ]
