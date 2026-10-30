module Waterfall.Sweep
( sweep
, trySweep
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire, emptyPaintMap, solidFromAcquireTWithCatch)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Edges (wireTangentStart, wireEndpoints, allWires, allSubShapesWithCopy)
import Waterfall.Internal.Finalizers (acquireForeignPtr)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Transforms (rotate, translate)
import Waterfall.Booleans (difference)
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipeShell as MakePipeShell
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.BRepTools as BRepTools
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import qualified OpenCascade.TopoDS as TopoDS
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr
import Linear (V3, normalize, unit, _x, _z, cross, dot)
import Data.Acquire (Acquire)
import Waterfall.Error (WaterfallError)
import Data.Either (fromRight)
import Control.Monad (void, forM, filterM)
import Data.Functor.Compose (Compose (..))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Data.Maybe (catMaybes)

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
                in acquireSolid . rotate axis angle . solidFromAcquire emptyPaintMap . pure $ face 

positionFace :: V3 Double -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
positionFace p = acquireSolid . translate p . solidFromAcquire emptyPaintMap . pure

reassembleSweep :: Compose [] NonEmpty Solid -> Solid
reassembleSweep = 
    let subtractTail (h :| t) = foldl' difference h t
    in mconcat . fmap subtractTail . getCompose

-- | Version of `sweep` that returns an Error on Failure
trySweep :: Path -> Shape -> Either WaterfallError Solid
trySweep (Path (ComplexRawPath theRawPath)) (Shape theRawShape) = fmap reassembleSweep . solidFromAcquireTWithCatch emptyPaintMap $ do
    path <- acquireForeignPtr theRawPath
    shape <- acquireForeignPtr theRawShape
    tangent <- liftIO $ wireTangentStart path
    (start,_)  <- liftIO $ wireEndpoints path
    adjustedFace <- positionFace start =<< rotateFace tangent shape
    faces <- allSubShapesWithCopy ShapeEnum.Face adjustedFace
    let shapeForWire wire = do
            builder <- MakePipeShell.new path
            liftIO $ do
                MakePipeShell.setDiscreteMode builder
                MakePipeShell.add builder wire False False
                MakeShape.build (upcast builder)
                void $ MakePipeShell.makeSolid builder
            MakeShape.shape (upcast builder)
    fmap (Compose . catMaybes) . forM faces $ \face -> do
        outerWire <- BRepTools.outerWire =<< liftIO (unsafeDowncast face)
        isNull <- liftIO $ TopoDS.Shape.isNull (upcast outerWire)
        if isNull
            then pure Nothing 
            else do
                innerWires <- filterM 
                    (liftIO 
                        . fmap not 
                        .  TopoDS.Shape.isSame (upcast outerWire)
                        . upcast
                    ) =<< allWires face
                (Just .) .  (:|) <$> shapeForWire outerWire
                    <*> forM innerWires shapeForWire
trySweep _ _ = Right mempty


-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
sweep :: Path -> Shape -> Solid
sweep path shape = fromRight mempty $ trySweep path shape