module Waterfall.Sweep
( sweep
) where

import Waterfall.Internal.Solid (Solid (..))
import Waterfall.Internal.Path (Path (..))
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipe as MakePipe
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)

-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
-- 
-- I may still need to make some tweaks to the alignment of the `Shape` and the `Path`.
-- This will likely impact the shapes that are generated when the start point of the `Path` 
-- is not parallel to the z axis.
sweep :: Path -> Shape -> Solid
sweep (Path runThePath) (Shape runTheShape) = Solid $ do
    path <- runThePath
    shape <- runTheShape
    builder <- MakePipe.fromWireAndShape path shape
    MakeShape.shape (upcast builder)
