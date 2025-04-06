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

-- | [Loft](https://en.wikipedia.org/wiki/Loft_\(3D\)) is a method to create smooth 3D shapes. 
--
-- Analagous to the [lofting](https://en.wikipedia.org/wiki/Lofting) process in boat building. 
-- A loft is defined by planar cross-sections of the desired shape at chosen locations. 
-- These cross-sections are then interpolated to form a smooth 3d shape.
--
-- This example demonstrates the `Loft` module, by generating a boat, with the profile of the boat specified by a series of bezier curves.
loftExample :: Solids.Solid
loftExample = 
    let paths = 
          [ let p x z = V3 x 0 z 
              -- the curve at the rear of the boat is tilted _slightly_ back
              in Transforms.rotate (V3 1 0 0) 0.2 $
                  Path.bezier (p 0 0) (p 4 0) (p 5 3) (p 5 4)
          , let p x z = V3 x 2 z 
              in  Path.bezier (p 0 0) (p 4 0) (p 5 3) (p 5 4)
          , let p x z = V3 x 5 z 
              in  Path.bezier (p 0 0) (p 4 0) (p 5 3) (p 5 4)
          , let p x z = V3 x 10 z
              in Path.bezier (p 1 0) (p 4.5 0) (p 5.25 3) (p 5.25 4.2)
          ]
        mirror = Transforms.mirror (V3 1 0 0 ) . Path.reversePath
        makeSymetric p = mirror p <> p
        symetricPaths = makeSymetric <$> paths
        body = 
          Loft.pointedLoft 
            Nothing
            (Path.closeLoop <$>  symetricPaths)
            (Just (V3 0 20 5))
        -- shrink the boat shape slightly, and translate it
        -- use this to hollow out the boat
        cavity = Transforms.translate (V3 0 (0.025 * 20) 0.3) $ Transforms.uScale 0.95 body
        -- sweep a circle along each of the paths, this makes them visible in the generated model
        sweepWithCircle = (`Sweep.sweep` Transforms2D.uScale2D 0.2 Shape.unitCircle)
        splines = mconcat $ sweepWithCircle <$> symetricPaths
      in Transforms.uScale 0.1 $
          (body <> splines) `Booleans.difference` cavity