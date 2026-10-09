{-# LANGUAGE TupleSections #-}
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

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquireTWithCatch, PaintMap (..), makeShapeHistory, solidFromAcquireTWithPaintMapWithCatch)
import qualified OpenCascade.BRepOffsetAPI.MakeOffsetShape as MakeOffsetShape
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (SubTypeOf(upcast), unsafeDowncast)
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.BRepOffset.Mode as Mode
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.TopExp.Explorer as TopExp.Explorer
import qualified OpenCascade.TopAbs.ShapeEnum as TopAbs.ShapeEnum
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

combineShellsToSolid :: Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
combineShellsToSolid s = do
    explorer <- TopExp.Explorer.new s TopAbs.ShapeEnum.Shell
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

getCompoundAsSolids :: Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Shape]
getCompoundAsSolids s = do
    explorer <-  TopExp.Explorer.new s TopAbs.ShapeEnum.Solid
    let go = do
            isMore <- liftIO $ TopExp.Explorer.more explorer
            if not isMore
                then pure []
                else  do
                    solid <- TopoDS.Shape.copy =<< liftIO (TopExp.Explorer.value explorer)
                    liftIO $ TopExp.Explorer.next explorer
                    (solid :) <$> go
    go


filterPaintMap :: Ptr TopoDS.Shape -> PaintMap -> Acquire PaintMap
filterPaintMap _ (UniformPaint paint) = pure (UniformPaint paint)
filterPaintMap s (FacePaints paints) = do
    faceSet <- liftIO . ShapeMap.fromList . fmap (,()) =<< allSubShapesWithCopy ShapeEnum.Face s 
    liftIO $ FacePaints <$> filterM (fmap isJust . ShapeMap.lookup faceSet . upcast . fst) paints
    
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


tryOffsetWithToleranceWithPaint :: 
    Double       
    -> (NonEmpty Paint -> Paint)
    -> Double
    -> Solid   
    -> Either WaterfallError Solid
tryOffsetWithToleranceWithPaint tolerance paintFn value solid
    | nearZero value = Right solid
    | otherwise = 
        fmap mconcat 
        . solidFromAcquireTWithPaintMapWithCatch
        $ traverse (offsetOneWithTolerance tolerance value paintFn (solidPaintMap solid)) 
        =<< getCompoundAsSolids 
        =<< acquireSolid solid

-- | Version of `offsetWithTolerance` that returns an error on failure
tryOffsetWithTolerance :: 
    Double       
    -> Double   
    -> Solid   
    -> Either WaterfallError Solid
tryOffsetWithTolerance tolerance 
    = tryOffsetWithToleranceWithPaint tolerance NE.head

offsetWithTolerance :: 
    Double       -- ^ Tolerance, this can be relatively small
    -> Double    -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid        -- ^ the `Solid` to offset 
    -> Solid
offsetWithTolerance tolerance value solid = 
    fromRight mempty $ tryOffsetWithTolerance tolerance value solid

    
offsetWithToleranceWithPaint :: 
    Double       -- ^ Tolerance, this can be relatively small
    -> (NonEmpty Paint -> Paint)
    -> Double    -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid        -- ^ the `Solid` to offset 
    -> Solid
offsetWithToleranceWithPaint tolerance paintFn value solid = 
    fromRight mempty $ tryOffsetWithToleranceWithPaint tolerance paintFn value solid

defaultTolerance :: Double
defaultTolerance = 1e-6

-- | Expand or contract a `Solid` by a certain amount.
-- 
-- This is based on @MakeOffsetShape@ from the underlying OpenCascade library.
-- And as such, only supports the same set of `Solid`s that @MakeOffsetShape@ does.
--
-- The documentation for @MakeOffsetShape@ lists the following constraints
-- ( [link](https://dev.opencascade.org/doc/refman/html/class_b_rep_offset_a_p_i___make_offset_shape.html) ):
--
-- * All the faces of the shape S should be based on the surfaces with continuity at least C1.
-- * The offset value should be sufficiently small to avoid self-intersections in resulting shape.
--      Otherwise these self-intersections may appear inside an offset face if its initial surface is not plane or sphere or cylinder, also some non-adjacent offset faces may intersect each other. Also, some offset surfaces may "turn inside out".
-- * The algorithm may fail if the shape S contains vertices where more than 3 edges converge.
-- * Since 3d-offset algorithm involves intersection of surfaces, it is under limitations of surface intersection algorithm.
-- * A result cannot be generated if the underlying geometry of S is BSpline with continuity C0.
offset :: 
    Double    -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid        -- ^ the `Solid` to offset 
    -> Solid
offset = offsetWithTolerance defaultTolerance

offsetWithPaint 
    :: (NonEmpty Paint -> Paint)
    -> Double 
    -> Solid
    -> Solid
offsetWithPaint = offsetWithToleranceWithPaint defaultTolerance

-- | Version of `offset` that returns an error on failure
tryOffset  :: 
    Double    -- ^ Amount to offset by, positive values expand, negative values contract
    -> Solid        -- ^ the `Solid` to offset 
    -> Either WaterfallError Solid
tryOffset = tryOffsetWithTolerance defaultTolerance

tryOffsetWithPaint 
    :: (NonEmpty Paint -> Paint)
    -> Double 
    -> Solid
    -> Either WaterfallError Solid
tryOffsetWithPaint = 
    tryOffsetWithToleranceWithPaint defaultTolerance 

