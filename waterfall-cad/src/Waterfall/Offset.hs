{-# LANGUAGE TupleSections #-}
{-|
Module: Waterfall.Offset

Expand or contract a `Solid` by a certain amount.

This is based on @MakeOffsetShape@ from the underlying OpenCascade library.
And as such, only supports the same set of `Solid`s that @MakeOffsetShape@ does.

The documentation for @MakeOffsetShape@ lists the following constraints
( [link](https://dev.opencascade.org/doc/refman/html/class_b_rep_offset_a_p_i___make_offset_shape.html) ):

* All the faces of the shape S should be based on the surfaces with continuity at least C1.
* The offset value should be sufficiently small to avoid self-intersections in resulting shape.
     Otherwise these self-intersections may appear inside an offset face if its initial surface is not plane or sphere or cylinder, also some non-adjacent offset faces may intersect each other. Also, some offset surfaces may "turn inside out".
* The algorithm may fail if the shape S contains vertices where more than 3 edges converge.
* Since 3d-offset algorithm involves intersection of surfaces, it is under limitations of surface intersection algorithm.
* A result cannot be generated if the underlying geometry of S is BSpline with continuity C0.

We work around the first and last of these constraints by splitting any face that is not C1 continuous along its discontinuities.
-}
module Waterfall.Offset
( offset
, offsetWithTolerance
, offsetWithPaint
, offsetWithToleranceWithPaint
-- * Functions that return Errors
, tryOffset
, tryOffsetWithTolerance
, tryOffsetWithPaint
, tryOffsetWithToleranceWithPaint
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, acquireFacePaints, fromAcquirePaints, PaintMap (..), makeShapeHistory, solidFromAcquireTWithPaintMapWithCatch, intersections3D, shapeDivideHistory, mapPaintMap, solidFromAcquireWithPaintMapWithCatch)
import qualified OpenCascade.BRepOffsetAPI.MakeOffsetShape as MakeOffsetShape
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (SubTypeOf(upcast), unsafeDowncast)
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.BRepOffset.Mode as Mode
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopExp.Explorer as TopExp.Explorer
import Control.Monad (when, filterM)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Error (WaterfallError)
import Data.Either (fromRight)
import Waterfall.Internal.Edges (allSubShapesWithCopy)
import qualified Waterfall.Internal.ShapeMap as ShapeMap
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import Data.Maybe (isJust)
import Waterfall.Internal.PaintHistory (remapWithBlendFunction)
import Data.List.NonEmpty (NonEmpty)
import Waterfall.Paint (Paint)
import qualified Data.List.NonEmpty as NE
import Data.Functor.Compose (Compose (..))
import qualified OpenCascade.ShapeUpgrade.ShapeDivideContinuity as ShapeUpgrade.ShapeDivideContinuity
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import qualified OpenCascade.ShapeUpgrade.ShapeDivide as ShapeUpgrade.ShapeDivide

combineShellsToSolid :: Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
combineShellsToSolid s = do
    explorer <- TopExp.Explorer.new s ShapeEnum.Shell
    makeSolid <- MakeSolid.new
    let go = do
            isMore <- liftIO $ TopExp.Explorer.more explorer
            when isMore $ do
                shell <- liftIO $ unsafeDowncast =<< TopExp.Explorer.value explorer
                liftIO $ MakeSolid.add makeSolid shell
                liftIO $ TopExp.Explorer.next explorer
                go
    go
    upcast <$> MakeSolid.solid makeSolid

splitC0BSplines' :: PaintMap -> Ptr TopoDS.Shape -> Acquire (PaintMap, Ptr TopoDS.Shape)
splitC0BSplines' paintMap s = do
    shapeDivide <- ShapeUpgrade.ShapeDivideContinuity.fromShape s
    liftIO $ ShapeUpgrade.ShapeDivideContinuity.setBoundaryCriterion shapeDivide GeomAbs.Shape.C1
    liftIO $ ShapeUpgrade.ShapeDivideContinuity.setSurfaceCriterion shapeDivide GeomAbs.Shape.C1
    liftIO $ ShapeUpgrade.ShapeDivideContinuity.setPCurveCriterion shapeDivide GeomAbs.Shape.C1
    performed <- liftIO $ ShapeUpgrade.ShapeDivide.perform (upcast shapeDivide) True
    if performed 
        then mapPaintMap (paintMap, shapeDivideHistory (upcast shapeDivide))
        else return (paintMap, s)

splitC0BSplines :: Solid -> Either WaterfallError Solid
splitC0BSplines solid = solidFromAcquireWithPaintMapWithCatch
    (splitC0BSplines' (solidPaintMap solid) =<< acquireSolid solid)

filterPaintMap :: Ptr TopoDS.Shape -> PaintMap -> Acquire PaintMap
filterPaintMap _ (UniformPaint paint) = pure (UniformPaint paint)
filterPaintMap s (FacePaints paints) = do
    faceSet <- liftIO . ShapeMap.fromList . fmap (,()) =<< allSubShapesWithCopy ShapeEnum.Face s
    fmap FacePaints . liftIO . fromAcquirePaints $
        ( filterM (liftIO . fmap isJust . ShapeMap.lookup faceSet . upcast . fst) 
            =<< acquireFacePaints paints
        )

offsetOneWithTolerance ::
    Double
    -> Double
    -> (NonEmpty Paint -> Paint)
    -> PaintMap
    -> Ptr TopoDS.Shape
    -> Acquire (PaintMap, Ptr TopoDS.Shape)
offsetOneWithTolerance tolerance value paintFn paintMap s = do
    builder <- MakeOffsetShape.new
    allEdges <- traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Edge s
    liftIO $ MakeOffsetShape.performByJoin builder s value tolerance Mode.Skin False False GeomAbs.JoinType.Arc False
    shell <- MakeShape.shape (upcast builder)
    newPaintMap <- remapWithBlendFunction paintFn s allEdges (makeShapeHistory $ upcast builder)
        =<< filterPaintMap s paintMap
    (newPaintMap,) <$> combineShellsToSolid shell

-- | Version of `tryOffsetWithTolerance` that exposes how to assign `Paint` to the generated parts of the surface
tryOffsetWithToleranceWithPaint
    :: Double -- ^ Tolerance, this can be relatively small
    -> (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a generated surface to the `Paint` used for the surface
    -> Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Either WaterfallError Solid
tryOffsetWithToleranceWithPaint tolerance paintFn value solid
    | nearZero value = Right solid
    | otherwise = do 
        continuousSolid <- splitC0BSplines solid
        fmap (mconcat . fmap intersections3D . getCompose)
            . solidFromAcquireTWithPaintMapWithCatch
            $ traverse (offsetOneWithTolerance tolerance value paintFn (solidPaintMap continuousSolid))
            =<< fmap Compose
            . traverse (allSubShapesWithCopy ShapeEnum.Shell)
            =<< allSubShapesWithCopy ShapeEnum.Solid
            =<< acquireSolid continuousSolid

-- | Version of `offsetWithTolerance` that returns an error on failure
tryOffsetWithTolerance
    :: Double -- ^ Tolerance, this can be relatively small
    -> Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Either WaterfallError Solid
tryOffsetWithTolerance tolerance
    = tryOffsetWithToleranceWithPaint tolerance NE.head

-- | Version of `offset` that exposes the tolerance value used by the offset algorithm
offsetWithTolerance
    :: Double -- ^  Tolerance, this can be relatively small
    -> Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Solid
offsetWithTolerance tolerance value solid =
    fromRight mempty $ tryOffsetWithTolerance tolerance value solid

-- | Version of `offsetWithTolerance` that exposes how to assign `Paint` to the generated parts of the surface
offsetWithToleranceWithPaint
    :: Double -- ^ Tolerance, this can be relatively small
    -> (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a generated surface to the `Paint` used for the surface
    -> Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Solid
offsetWithToleranceWithPaint tolerance paintFn value solid =
    fromRight mempty $ tryOffsetWithToleranceWithPaint tolerance paintFn value solid

defaultTolerance :: Double
defaultTolerance = 1e-6

-- | Expand or contract a `Solid` by a certain amount.
offset
    :: Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Solid
offset = offsetWithTolerance defaultTolerance


-- | Version of `offset` that exposes how to assign `Paint` to the generated parts of the surface
offsetWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a generated surface to the `Paint` used for the surface
    -> Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Solid
offsetWithPaint = offsetWithToleranceWithPaint defaultTolerance

-- | Version of `offset` that returns an error on failure
tryOffset
    :: Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Either WaterfallError Solid
tryOffset = tryOffsetWithTolerance defaultTolerance

-- | Version of `tryOffset` that exposes how to assign `Paint` to the generated parts of the surface
tryOffsetWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a generated surface to the `Paint` used for the surface
    -> Double -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid -- ^ The `Solid` to offset
    -> Either WaterfallError Solid
tryOffsetWithPaint =
    tryOffsetWithToleranceWithPaint defaultTolerance