module Waterfall.Sweep
( sweep
) where

import Waterfall.Internal.Solid (Solid (..))
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Edges (wireTangent, wireEndpoints)
import Waterfall.Transforms (rotate, translate)
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipe as MakePipe
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)
import qualified OpenCascade.TopoDS as TopoDS
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr
import Linear (V3, normalize, unit, _x, _z, nearZero, cross, dot)
import Data.Acquire (Acquire)

rotateFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
rotateFace v face = 
    let vn = normalize v
        z = unit _z
        in if nearZero (vn - z)
            then pure face
            else
                let axis = if nearZero (vn + z) then unit _x else cross vn z
                    angle = acos (dot vn z) 
                in runSolid . rotate axis angle . Solid . pure $ face 

positionFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
positionFace p = runSolid . translate p . Solid . pure

-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
sweep :: Path -> Shape -> Solid
sweep (Path runThePath) (Shape runTheShape) = Solid $ do
    path <- runThePath
    shape <- runTheShape
    tangent <- liftIO $ wireTangent path
    (start,_)  <- liftIO $ wireEndpoints path
    adjustedFace <- positionFace start =<< rotateFace tangent shape
    builder <- MakePipe.fromWireAndShape path adjustedFace
    MakeShape.shape (upcast builder)
