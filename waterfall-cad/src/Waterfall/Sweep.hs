module Waterfall.Sweep
( sweep
, trySweep
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire, solidFromAcquireWithCatch)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Edges (wireTangentStart, wireEndpoints)
import Waterfall.Internal.Finalizers (toAcquire)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Transforms (rotate, translate)
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipe as MakePipe
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)
import qualified OpenCascade.TopoDS as TopoDS
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr
import Linear (V3, normalize, unit, _x, _z, cross, dot)
import Data.Acquire (Acquire)
import Waterfall.Error (WaterfallError)
import Data.Either (fromRight)

rotateFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
rotateFace v face = 
    let vn = normalize v
        z = unit _z
        in if all nearZero (vn - z)
            then pure face
            else
                let axis = 
                        if all nearZero (vn + z)
                            then unit _x 
                            else z `cross` vn
                    angle = acos (vn `dot` z)
                in acquireSolid . rotate axis angle . solidFromAcquire . pure $ face 

positionFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
positionFace p = acquireSolid . translate p . solidFromAcquire . pure


-- | Version of `sweep` that returns an Error on Failure
trySweep :: Path -> Shape -> Either WaterfallError Solid
trySweep (Path (ComplexRawPath theRawPath)) (Shape theRawShape) = solidFromAcquireWithCatch $ do
    path <- toAcquire theRawPath
    shape <- toAcquire theRawShape
    tangent <- liftIO $ wireTangentStart path
    (start,_)  <- liftIO $ wireEndpoints path
    adjustedFace <- positionFace start =<< rotateFace tangent shape
    builder <- MakePipe.fromWireAndShape path adjustedFace
    MakeShape.shape (upcast builder)
trySweep _ _ = Right mempty


-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
sweep :: Path -> Shape -> Solid
sweep path shape = fromRight mempty $ trySweep path shape