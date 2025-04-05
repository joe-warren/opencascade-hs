module Waterfall.Sweep
( sweep
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Edges (wireTangentStart, wireEndpoints)
import Waterfall.Internal.Finalizers (toAcquire)
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
import qualified Waterfall.Solids as Solids

rotateFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
rotateFace v face = 
    let vn = normalize v
        z = unit _z
        in if nearZero (vn - z)
            then pure face
            else
                let axis = if nearZero (vn + z) then unit _x else z `cross` vn
                    angle = acos (vn `dot` z)
                in acquireSolid . rotate axis angle . solidFromAcquire . pure $ face 

positionFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
positionFace p = acquireSolid . translate p . solidFromAcquire . pure

-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
sweep :: Path -> Shape -> Solid
sweep (Path (ComplexRawPath theRawPath)) (Shape theRawShape) = solidFromAcquire $ do
    path <- toAcquire theRawPath
    shape <- toAcquire theRawShape
    tangent <- liftIO $ wireTangentStart path
    (start,_)  <- liftIO $ wireEndpoints path
    adjustedFace <- positionFace start =<< rotateFace tangent shape
    builder <- MakePipe.fromWireAndShape path adjustedFace
    MakeShape.shape (upcast builder)
sweep _ _ = Solids.nowhere
