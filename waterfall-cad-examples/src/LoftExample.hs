module LoftExample 
( loftExample
) where

import Linear (V3 (..))
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as Transforms2D
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Solids as Solids
import qualified Waterfall.Sweep as Sweep
import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Loft as Loft
import qualified Waterfall.Path.Common as Path

loftExample :: Solids.Solid
loftExample = 
    let precision = 1e-6
        paths = [ let p x z = V3 x 0 z 
              in Transforms.rotate (V3 1 0 0) 0.4 $ Path.pathFrom (p 0 0) 
                    [ Path.lineTo (p 2 0)
                    , Path.bezierTo (p 4 0) (p 5 3) (p 5 4)
                    , Path.lineTo (p (-5) 4)
                    , Path.bezierTo (p (-5) 3) (p (-4) 0) (p (-2) 0)
                    , Path.lineTo (p 0 0)
                    ]
            , let p x z = V3 x 1 z 
              in Path.pathFrom (p 0 0) 
                    [ Path.lineTo (p 2 0)
                    , Path.bezierTo (p 4 0) (p 5 3) (p 5 4)
                    , Path.lineTo (p (-5) 4)
                    , Path.bezierTo (p (-5) 3) (p (-4) 0) (p (-2) 0)
                    , Path.lineTo (p 0 0)
                    ]

            , let p x z = V3 x 20 z 
              in Path.pathFrom (p 0 0) 
                    [ Path.lineTo (p 5 0)
                    , Path.bezierTo (p 6 0) (p 6 3) (p 6 4)
                    , Path.lineTo (p (-6) 4)
                    , Path.bezierTo (p (-6) 3) (p (-6) 0) (p (-5) 0)
                    , Path.lineTo (p 0 0)
                    ]
            ]
        body = 
          Loft.pointedLoft 
            precision 
            Nothing
            paths (Just (V3 0 30 6))
        in body `Booleans.difference` (Transforms.translate (V3 0 (0.025 * 30) 0.5) $ Transforms.uScale 0.95 body)