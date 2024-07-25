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

-- Build a boat, with the profile of the boat defined using a series of bezier curves
loftExample :: Solids.Solid
loftExample = 
    let precision = 1e-6
        paths = 
          [ let p x z = V3 x 0 z 
              -- the curve at the rear of the boat is tilted _slightly_ back
              in Transforms.rotate (V3 1 0 0) 0.2 $
                  Path.bezier (p 0 0) (p 4 0) (p 5 3) (p 5 4)
          , let p x z = V3 x 2 z 
              in  Path.bezier (p 0 0) (p 4 0) (p 5 3) (p 5 4)
          , let p x z = V3 x 7.5 z 
              in  Path.bezier (p 0 0) (p 4 0) (p 5 3) (p 5 4)
          , let p x z = V3 x 20 z
              in Path.bezier (p 0 0) (p 5 0) (p 5.5 3) (p 5.5 4.2)
          ]
        mirror = Path.reversePath . Transforms.mirror (V3 1 0 0 )
        makeSymetric p = mirror p <> p
        symetricPaths = makeSymetric <$> paths
        body = 
          Loft.pointedLoft 
            precision 
            Nothing
            ( Path.closeLoop <$>  symetricPaths)
            (Just (V3 0 30 5))
        -- shrink the boat shape slightly, and translate it
        -- use this to hollow out the boat
        cavity = Transforms.translate (V3 0 (0.025 * 30) 0.3) $ Transforms.uScale 0.95 body
        -- sweep a circle along each of the paths, this makes them visible in the generated model
        sweepWithCircle = (`Sweep.sweep` Transforms2D.uScale2D 0.2 Shape.unitCircle)
        splines = mconcat $ sweepWithCircle <$> symetricPaths
      in Transforms.uScale 0.1 $
          (body <> splines) `Booleans.difference` cavity