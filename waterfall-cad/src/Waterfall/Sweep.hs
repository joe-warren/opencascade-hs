module Waterfall.Sweep
( sweep
) where

import Waterfall.Internal.Solid (Solid (..))
import Waterfall.Internal.Path (Path (..))
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipe as MakePipe
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)

sweep :: Path -> Shape -> Solid
sweep (Path runPath) (Shape runShape) = Solid $ do
    path <- runPath
    shape <- runShape
    builder <- MakePipe.fromWireAndShape path shape
    MakeShape.shape (upcast builder)
