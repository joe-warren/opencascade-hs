{-# LANGUAGE TupleSections #-}
module Waterfall.Internal.Diagram
( buildSolidDiagramFills
) where
import Waterfall.Internal.Solid (PaintMap(..), materialiseFacePaints, paintMapIsEmpty)
import Waterfall.Paint (Paint)
import Waterfall.TwoD.Internal.Shape (Shape (..))
import Data.Acquire (Acquire)
import Foreign (Ptr)
import qualified OpenCascade.HLRBRep.Types as HLRBRep
import qualified OpenCascade.HLRBRep.TypeOfResultingEdge as HLRBRep
import qualified OpenCascade.TopoDS as TopoDS
import Waterfall.Internal.Edges (allEdges, buildEdgeCurve3D, allSubShapesWithCopy)
import Data.Traversable (forM)
import qualified OpenCascade.HLRBRep.HLRToShape as HLRBRep.HLRToShape
import qualified OpenCascade.Bnd.Box as Bnd.Box
import Data.Foldable (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.BRepBndLib as BRepBndLib
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo.Builder
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopoDS.Builder as TopoDS.Builder
import qualified OpenCascade.TopoDS.Compound as TopoDS.Compound
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import Control.Lens ((^.), (&), (.~), (<&>))
import Linear (_x, _y, _z, V3, norm)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe, catMaybes)
import qualified Waterfall.Internal.ShapeMap as ShapeMap
import qualified OpenCascade.BOPAlgo.Tools as BOPAlgo.Tools
import qualified OpenCascade.HLRAlgo as HLRAlgo
import qualified OpenCascade.HLRAlgo.Projector as Projector
import qualified OpenCascade.IntCurvesFace.ShapeIntersector as ShapeIntersector
import Control.Arrow (first)
import qualified OpenCascade.BRepMesh.IncrementalMesh as IncrementalMesh
import qualified OpenCascade.TopLoc.Location as TopLoc.Location
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.BRepGProp as BRepGProp
import qualified OpenCascade.GProp.GProps as GProps
import qualified OpenCascade.Poly.Triangulation as Poly.Triangulation
import qualified OpenCascade.Poly.Triangle as Poly.Triangle
import Control.Monad ((<=<), filterM)
import Waterfall.Internal.ToOpenCascade (v3ToPnt, v3ToDir)
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Lin as GP.Lin
import Waterfall.Internal.Finalizers (fromAcquirePtr, fromAcquirePtrT)
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified Data.Map as M
import qualified OpenCascade.ShapeUpgrade.UnifySameDomain as UnifySameDomain

boundaryEdges :: Ptr HLRBRep.HLRToShape -> Acquire [Ptr TopoDS.Edge]
boundaryEdges extractor = 
    fmap concat $ forM [HLRBRep.OutLine, HLRBRep.Sharp, HLRBRep.Rg1Line] $ \lt -> do
        compoundOfEdges <- HLRBRep.HLRToShape.compoundOfEdges 
            extractor lt True False
        traverse buildEdgeCurve3D =<< allEdges compoundOfEdges

calcFuzzyTolerance :: [Ptr TopoDS.Edge] -> Acquire Double
calcFuzzyTolerance edges = do
    theBox <- Bnd.Box.new
    forM_ edges $ \e -> do
        liftIO $ BRepBndLib.addOptimal (upcast e) theBox True False

    lo <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMin theBox
    hi <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMax theBox
    let d = hi - lo
    let maxDim = max (d ^. _x) (d ^. _y)
    return $ max 1e-6 (maxDim / 10000)

calcRayBound :: Ptr TopoDS.Shape -> Acquire Double
calcRayBound s = do
    theBox <- Bnd.Box.new
    liftIO $ BRepBndLib.addOptimal s theBox True False

    lo <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMin theBox
    hi <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMax theBox
    return ( 2 * (norm lo + norm hi))

-- recursively drop edges with a free end
pruneDangling :: [Ptr TopoDS.Edge] -> Acquire [Ptr TopoDS.Edge]
pruneDangling edges = do
    withVertices <- forM edges $ \e -> do
        (e, ) <$> allSubShapesWithCopy ShapeEnum.Vertex (upcast e)
    let loop current = do
            vertexIndex <- liftIO $ ShapeMap.fromList (zip (concatMap snd current) [0 :: Int ..])
            labelled <- forM current $ \(e, vs) ->
                fmap (e, vs,) . liftIO $ traverse (fmap (fromMaybe (-1)) . ShapeMap.lookup vertexIndex) vs
            let weight labels = if length labels == 1 then 2 else 1 :: Int
                counts = IntMap.fromListWith (+) [(l, weight labels) | (_, _, labels) <- labelled, l <- labels]
                keep = [ (e, vs) | (e, vs, labels) <- labelled, all (\l -> IntMap.findWithDefault 0 l counts >= 2) labels ]
            if length keep == length current then pure (fst <$> keep) else loop keep
    loop withVertices

mergeRegions :: [Ptr TopoDS.Face] -> Acquire [Shape]
mergeRegions [] = pure []
mergeRegions [s] = liftIO $ pure . Shape <$> fromAcquirePtr (TopoDS.Shape.copy . upcast $ s)
mergeRegions regions = do
    builder <- TopoDS.Builder.new
    compound <- TopoDS.Compound.new
    liftIO $ TopoDS.Builder.makeCompound builder compound
    forM_ regions $ liftIO . TopoDS.Builder.add builder (upcast compound) . upcast
    unifier <- UnifySameDomain.fromShape (upcast compound) False True False
    liftIO $ UnifySameDomain.build unifier
    sameDomain <- UnifySameDomain.shape unifier
    liftIO . fmap (fmap Shape) . fromAcquirePtrT $ allSubShapesWithCopy ShapeEnum.Face sameDomain

faceArea :: Ptr TopoDS.Face -> Acquire Double
faceArea face = do
    props <- GProps.new
    liftIO $ BRepGProp.surfaceProperties (upcast face) props False False
    liftIO $ GProps.mass props

buildSolidDiagramFills' 
    :: Ptr TopoDS.Shape
    -> V3 Double 
    -> Ptr HLRAlgo.Projector
    -> Ptr HLRBRep.HLRToShape
    -> [(Ptr TopoDS.Face, Paint)]
    -> Acquire [(Paint, Shape)]
buildSolidDiagramFills' s projectionDir projector extractor paints = do
    boundary <- boundaryEdges extractor
    if null boundary 
        then return []
        else do
            tolerance <- calcFuzzyTolerance boundary 
            rayBound <- calcRayBound s
            let angularTolerance = 1e-8
            fuser <- BOPAlgo.Builder.new 
            liftIO $ BOPAlgo.Builder.setFuzzyValue fuser tolerance
            forM_ boundary (liftIO . BOPAlgo.Builder.addArgument fuser . upcast)
            liftIO $ BOPAlgo.Builder.perform fuser
            keptEdges <- pruneDangling 
                =<< traverse (liftIO . unsafeDowncast)
                =<<  allSubShapesWithCopy ShapeEnum.Edge 
                =<< BOPAlgo.Builder.shape fuser

            builder <- TopoDS.Builder.new
            compound <- TopoDS.Compound.new
            liftIO $ TopoDS.Builder.makeCompound builder compound
            forM_ keptEdges $ liftIO . TopoDS.Builder.add builder (upcast compound) . upcast

            wires <- TopoDS.Shape.new
            status <- liftIO $ BOPAlgo.Tools.edgesToWires (upcast compound) wires True angularTolerance
            if status /= 0
                then pure [] 
                else do
                    faces <- TopoDS.Shape.new
                    ok <- liftIO $ BOPAlgo.Tools.wiresToFaces wires faces angularTolerance
                    if not ok
                        then return [] 
                        else do
                            regions <- traverse (liftIO . unsafeDowncast)
                                 =<< allSubShapesWithCopy ShapeEnum.Face faces

                            inv <- Projector.invertedTransformation projector
                            intersector <- ShapeIntersector.new
                            liftIO $ ShapeIntersector.load intersector s (tolerance/1000)

                            paintMap <- liftIO $ ShapeMap.fromList (first upcast <$> paints)
                            
                            d <- v3ToDir projectionDir

                            forwardFacingRegions <- filterM (fmap (> 0) . faceArea) regions
                            paintRegionMap <- fmap (M.fromListWith (<>) . catMaybes) $ forM forwardFacingRegions $ \region -> do
                                mesh <- IncrementalMesh.fromShapeAndLinDeflection (upcast region) tolerance
                                liftIO $ IncrementalMesh.perform mesh
                                loc <- TopLoc.Location.new
                                props <- GProps.new
                                liftIO $ BRepGProp.surfaceProperties (upcast region) props False False 
                                tri <- BRep.Tool.triangulation region loc
                                nullTriangulation <- liftIO $ Poly.Triangulation.isNull tri
                                nTri <- if nullTriangulation then pure 0 else liftIO $ Poly.Triangulation.nbTriangles tri

                                triangleCenters <- forM [1..nTri] $ \i -> do 
                                    t <- Poly.Triangulation.triangle tri i
                                    indices <- traverse (liftIO . Poly.Triangle.value t) [1, 2, 3]
                                    corners <- traverse (liftIO . gpPntToV3 <=< Poly.Triangulation.node tri) indices
                                    pure (sum corners / 3)
                                centerOfMass <- liftIO . gpPntToV3 =<< GProps.centreOfMass props
                                let candidatePoints = (triangleCenters <> [centerOfMass])
                                        & (traverse . _z .~ 0)

                                let lookupCandidatePaint [] = pure Nothing
                                    lookupCandidatePaint (h:t) = do
                                        pin <- v3ToPnt h
                                        p <- GP.Pnt.transformed pin inv
                                        line <- GP.Lin.new p d
                                        liftIO $ ShapeIntersector.perform intersector line (-rayBound) rayBound
                                        
                                        n <- liftIO $ ShapeIntersector.nbPnt intersector
                                        hits <- forM [1 .. n] $ \i -> (, i) <$> liftIO (ShapeIntersector.wParameter intersector i)
                                        case hits of
                                            [] -> lookupCandidatePaint t
                                            _ -> do
                                                let (_, nearest) = maximum hits
                                                hit <- ShapeIntersector.face intersector nearest
                                                found <- liftIO $ ShapeMap.lookup paintMap (upcast hit)
                                                return $ Just (fromMaybe mempty found)
                                paint <- lookupCandidatePaint candidatePoints
                                return $ (, [region]) <$> paint

                            traverse mergeRegions paintRegionMap
                                <&> foldMap sequence . M.toList

buildSolidDiagramFills
    :: PaintMap 
    -> Ptr TopoDS.Shape 
    -> V3 Double
    -> Ptr HLRAlgo.Projector
    -> Ptr HLRBRep.HLRToShape
    -> Acquire [(Paint, Shape)]
buildSolidDiagramFills paints s d projector extractor
    | paintMapIsEmpty paints = pure []
    | otherwise =    
        buildSolidDiagramFills' s d projector extractor
        =<< materialiseFacePaints s paints 