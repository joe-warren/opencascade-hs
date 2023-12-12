module Waterfall.Sweep
( sweep
) where

import Waterfall.Internal.Solid (Solid (..))
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Edges (wireTangent)
import Waterfall.Transforms (rotate)
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipe as MakePipe
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)
import qualified OpenCascade.TopoDS as TopoDS
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr
import Linear (V3, normalize, unit, _x, _z, nearZero, cross, dot)
import Data.Acquire (Acquire)

adjustFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
adjustFace v face = 
    let vn = normalize v
        z = unit _z
        in if nearZero (vn - z)
            then pure face
            else
                let axis = if nearZero (vn + z) then unit _x else cross vn z
                    angle = acos (dot vn z) 
                in runSolid . rotate axis angle . Solid . pure $ face 

-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
-- 
-- I may still need to make some tweaks to the alignment of the `Shape` and the `Path`.
-- This will likely impact the shapes that are generated when the start point of the `Path` 
-- is not parallel to the z axis.
sweep :: Path -> Shape -> Solid
sweep (Path runThePath) (Shape runTheShape) = Solid $ do
    path <- runThePath
    shape <- runTheShape
    tangent <- liftIO $ wireTangent path
    adjustedFace <- adjustFace tangent shape
    builder <- MakePipe.fromWireAndShape path adjustedFace
    MakeShape.shape (upcast builder)
