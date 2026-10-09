{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Waterfall.Internal.PaintHistory
( remapWithBlendFunction
) where

import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire)
import Waterfall.Paint (Paint)
import Control.Monad.IO.Class (liftIO)
import Waterfall.Internal.Solid (remapPaints, History (historyGenerated), acquirePaints, PaintMap (FacePaints, UniformPaint), materialiseFacePaints)
import OpenCascade.Inheritance (SubTypeOf(upcast), unsafeDowncast)
import qualified OpenCascade.NCollection as NCollection
import qualified Waterfall.Internal.ShapeMap as ShapeMap
import Waterfall.Internal.Edges (allSubShapesWithCopy)
import Data.Foldable (toList)
import qualified OpenCascade.NCollection.IndexedDataMap as NCollection.IndexedDataMap
import qualified OpenCascade.TopExp as TopExp
import Control.Arrow (first)
import qualified OpenCascade.NCollection.List as NCollection.List
import Data.Maybe (catMaybes)
import qualified Data.List.NonEmpty as NE
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Control.Monad (filterM)

getUniqueVertexes :: [Ptr TopoDS.Edge] -> Acquire [Ptr TopoDS.Vertex]
getUniqueVertexes edges = 
    let vertsForEdge e = traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Vertex (upcast e)
        dup a = (upcast a, a)
        in fmap toList . liftIO . ShapeMap.fromList . fmap dup . concat  =<< traverse vertsForEdge edges

makeAncestorMap :: Ptr TopoDS.Shape -> Acquire (Ptr (NCollection.IndexedDataMap TopoDS.Shape (NCollection.List TopoDS.Shape)))
makeAncestorMap s = do
    m <- NCollection.IndexedDataMap.newShapeListOfShapeMap
    liftIO $ TopExp.mapShapesAndAncestors s ShapeEnum.Edge ShapeEnum.Face m
    liftIO $ TopExp.mapShapesAndAncestors s ShapeEnum.Vertex ShapeEnum.Face m
    return m

remapWithBlendFunction' :: (NE.NonEmpty Paint -> Paint) -> Ptr TopoDS.Shape -> [Ptr TopoDS.Edge] -> History -> [(Ptr TopoDS.Face, Paint)] -> Acquire PaintMap
remapWithBlendFunction' paintFunction s edges history paints = do
    mappedPaints <- liftIO $ remapPaints history paints

    ancestorMap <- makeAncestorMap s
    paintMap <- liftIO $ ShapeMap.fromList (fmap (first upcast) paints)

    let filletSurfacesFor :: Ptr TopoDS.Shape -> Acquire [(Ptr TopoDS.Face, Paint)]
        filletSurfacesFor edge = do
            faces <- liftIO . filterM (fmap (== ShapeEnum.Face) . TopoDS.Shape.shapeType)
                =<< NCollection.List.fromListOfShape 
                =<< historyGenerated history edge
            if null faces then pure [] else do
                neighbours <- NCollection.List.fromListOfShape =<<
                    NCollection.IndexedDataMap.findFromKeyShapeListOfShapeMap ancestorMap edge
                neighbouringPaints <- liftIO . fmap catMaybes $ traverse (ShapeMap.lookup paintMap) neighbours
                case NE.nonEmpty neighbouringPaints of 
                    Nothing -> pure []
                    Just ps -> 
                        let newPaint = paintFunction (NE.nub . NE.sort $ ps)
                        in traverse (fmap (, newPaint) . liftIO . unsafeDowncast) faces

    filletSurfacesEdges <- liftIO . acquirePaints $ concat <$> traverse (filletSurfacesFor . upcast) edges
    verts <- getUniqueVertexes edges
    filletSurfacesVerts <-liftIO . acquirePaints $ concat <$> traverse (filletSurfacesFor . upcast) verts

    return $ FacePaints (mappedPaints <> filletSurfacesEdges <> filletSurfacesVerts)


remapWithBlendFunction :: (NE.NonEmpty Paint -> Paint) -> Ptr TopoDS.Shape -> [Ptr TopoDS.Edge] -> History -> PaintMap -> Acquire PaintMap
remapWithBlendFunction paintFunction s edges history (FacePaints paints) = 
    remapWithBlendFunction' paintFunction s edges history paints
remapWithBlendFunction paintFunction s edges history (UniformPaint paint) 
    | paintFunction (NE.singleton paint) == paint = pure $ UniformPaint paint
    | otherwise = remapWithBlendFunction' paintFunction s edges history =<< materialiseFacePaints s (UniformPaint paint) 

