{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Waterfall.Fillet
(
 -- * Rounds
-- | Fillet that adds radiused faces that are tangent to the two faces either side of an edge.
-- 
-- Sometimes, it may not be possible to construct a fillet because there is not enough space next to one of the fillet edges,
-- Or because the geometry is too complicated for the fillet algorithm.
  roundFillet
, roundConditionalFillet
, roundIndexedConditionalFillet
, tryRoundFillet
, tryRoundConditionalFillet
, tryRoundIndexedConditionalFillet
, tryRoundIndexedConditionalFilletWithPaint
-- * Chamfers
-- | Adds flat faces at a constant angle to the two faces either side of an edge.
, chamfer
, conditionalChamfer
, indexedConditionalChamfer
, tryChamfer
, tryConditionalChamfer
, tryIndexedConditionalChamfer
-- * Utility Methods
, whenNearlyEqual
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquireWithCatch, makeShapeHistory, remapPaints, PaintMap (..), solidFromAcquireMappingPaintMapWithCatch, solidFromAcquireWithPaintMapWithCatch, acquirePaints, materialiseFacePaints)
import Waterfall.Internal.Edges (edgeEndpoints, allEdges, allSubShapesWithCopy)
import Waterfall.Error (WaterfallError)
import qualified OpenCascade.BRepFilletAPI.MakeFillet as MakeFillet
import qualified OpenCascade.BRepFilletAPI.MakeChamfer as MakeChamfer
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopExp.Explorer as Explorer 
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopTools.ShapeMapHasher as TopTools.ShapeMapHasher
import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Control.Monad (when, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, unsafeDowncast, SubTypeOf)
import Linear.V3 (V3 (..))
import Linear.Epsilon (Epsilon, nearZero)
import Control.Lens (Lens', (^.))
import Data.Either (fromRight)
import Waterfall.Paint (Paint)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Acquire (Acquire)
import Data.Maybe (catMaybes)
import Data.Foldable (find, fold, toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified OpenCascade.NCollection as NCollection
import qualified OpenCascade.NCollection.IndexedDataMap as NCollection.IndexedDataMap
import qualified OpenCascade.TopExp as TopExp
import qualified OpenCascade.TopAbs.ShapeEnum as TopAbs.ShapeEnum
import qualified OpenCascade.NCollection.List as NCollection.List

addEdges :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> (Double -> Ptr TopoDS.Edge -> IO ()) -> Ptr Explorer.Explorer -> IO ()
addEdges radiusFn action explorer = go [] 0
    where go visited i = do
            isMore <- Explorer.more explorer
            when isMore $ do
                v <- unsafeDowncast =<< Explorer.value explorer
                hash <- TopTools.ShapeMapHasher.hash (upcast v)
                if hash `elem` visited
                    then do
                        Explorer.next explorer
                        go visited i
                    else do
                        endpoints <- edgeEndpoints v
                        case radiusFn i endpoints of 
                            Just r | r > 0 -> action r v
                            _ -> pure ()
                        Explorer.next explorer
                        go (hash:visited) (i + 1) 

getEdgesWithRadius :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Ptr TopoDS.Shape -> Acquire [(Double, Ptr TopoDS.Edge)]
getEdgesWithRadius f s = do
    edges <- allEdges s
    fmap catMaybes . forM (zip [0..] edges) $ \(i, e) -> do
        endpoints <- liftIO $ edgeEndpoints e
        return $ (, e) <$> find (> 0) (f i endpoints)

buildShapeLookup :: (SubTypeOf TopoDS.Shape a) => [(Ptr a, b)] -> IO (IntMap b)
buildShapeLookup paints = 
    fmap IntMap.fromList $ forM paints $ \(face, paint) -> do
        hash <- TopTools.ShapeMapHasher.hash (upcast face)
        return (hash, paint)

lookupShape :: IntMap a -> Ptr TopoDS.Shape -> IO (Maybe a)
lookupShape m s = (`IntMap.lookup` m) <$> TopTools.ShapeMapHasher.hash s

getUniqueVertexes :: [Ptr TopoDS.Edge] -> Acquire [Ptr TopoDS.Vertex]
getUniqueVertexes edges = 
    let vertsForEdge e = traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy TopAbs.ShapeEnum.Vertex (upcast e)
        dup a = (a, a)
        in fmap (toList . fold) . liftIO . traverse (buildShapeLookup . fmap dup) =<< traverse vertsForEdge edges

makeAncestorMap :: Ptr TopoDS.Shape -> Acquire (Ptr (NCollection.IndexedDataMap TopoDS.Shape (NCollection.List TopoDS.Shape)))
makeAncestorMap s = do
    m <- NCollection.IndexedDataMap.newShapeListOfShapeMap
    liftIO $ TopExp.mapShapesAndAncestors s TopAbs.ShapeEnum.Edge TopAbs.ShapeEnum.Face m
    liftIO $ TopExp.mapShapesAndAncestors s TopAbs.ShapeEnum.Vertex TopAbs.ShapeEnum.Face m
    return m
                        
tryRoundIndexedConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint)
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFilletWithPaint paintFunction radiusFunction solid = 
    solidFromAcquireWithPaintMapWithCatch $ do
        s <- acquireSolid solid
        builder <- MakeFillet.fromShape s

        edgesWithRadius <- getEdgesWithRadius radiusFunction s

        liftIO $ forM_ edgesWithRadius (uncurry (MakeFillet.addEdgeWithRadius builder))

        resultShape <- MakeShape.shape (upcast builder)

        let buildPaintMapFrom paints = do
                mappedPaints <- liftIO $ remapPaints (makeShapeHistory . upcast $ builder) paints

                ancestorMap <- makeAncestorMap s
                paintMap <- liftIO $ buildShapeLookup paints 

                let filletSurfacesFor :: Ptr TopoDS.Shape -> Acquire [(Ptr TopoDS.Face, Paint)]
                    filletSurfacesFor edge = do
                        faces <- NCollection.List.fromListOfShape =<< MakeShape.generated (upcast builder) edge
                        if null faces then pure [] else do
                            neighbours <- NCollection.List.fromListOfShape =<<
                                NCollection.IndexedDataMap.findFromKeyShapeListOfShapeMap ancestorMap edge
                            neighbouringPaints <- liftIO . fmap catMaybes $ traverse (lookupShape paintMap) neighbours
                            case NE.nonEmpty neighbouringPaints of 
                                Nothing -> pure []
                                Just ps -> 
                                    let newPaint = paintFunction (NE.nub . NE.sort $ ps)
                                    in traverse (fmap (, newPaint) . liftIO . unsafeDowncast) faces

                filletSurfacesEdges <- liftIO . acquirePaints $ concat <$> traverse (filletSurfacesFor . upcast . snd) edgesWithRadius
                verts <- getUniqueVertexes (snd <$> edgesWithRadius)
                filletSurfacesVerts <-liftIO . acquirePaints $ concat <$> traverse (filletSurfacesFor . upcast) verts

                return $ FacePaints (mappedPaints <> filletSurfacesEdges <> filletSurfacesVerts)

        paintMapWithoutFilletFaces <- case solidPaintMap solid of
            UniformPaint p -> 
                if paintFunction (NE.singleton p) == p 
                    then pure $ UniformPaint p
                    else buildPaintMapFrom =<< materialiseFacePaints s (UniformPaint p) 
            FacePaints paints -> buildPaintMapFrom paints

        pure (paintMapWithoutFilletFaces, resultShape)
--}
-- | Version of `roundIndexedConditionalFillet` that returns an `Either` on failure
tryRoundIndexedConditionalFillet
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFillet radiusFunction solid = solidFromAcquireWithCatch (solidPaintMap solid) $ do
    s <- acquireSolid solid
    builder <- MakeFillet.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdges radiusFunction (MakeFillet.addEdgeWithRadius builder) explorer

    MakeShape.shape (upcast builder)

-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge, and the index of the edge.
-- 
-- This can be used to selectively round\/fillet a `Solid`.
--
-- In general, relying on the edge index is inelegant,
-- however, if you consider a Solid with a semicircular face, 
-- there's no way to select either the curved or the flat edge of the semicircle based on just the endpoints.
--
-- Being able to selectively round\/fillet based on edge index is an \"easy\" way to round\/fillet these shapes. 
roundIndexedConditionalFillet
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Solid
roundIndexedConditionalFillet radiusFunction solid = fromRight mempty $ tryRoundIndexedConditionalFillet radiusFunction solid


-- | Version of `roundConditionalFillet` that returns an `Either` on failure
tryRoundConditionalFillet 
    :: ((V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Either WaterfallError Solid
tryRoundConditionalFillet f = tryRoundIndexedConditionalFillet (const f)

-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge.
-- 
-- This can be used to selectively round\/fillet a `Solid`.
roundConditionalFillet :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundConditionalFillet f = roundIndexedConditionalFillet (const f)

-- | Add a round with a given radius to every edge of a solid
--
-- Because this is applied to both internal (concave) and external (convex) edges, it may technically produce both Rounds and Fillets
roundFillet :: Double -> Solid -> Solid
roundFillet r = roundConditionalFillet (const . pure $ r)


-- | Version of `roundFillet` that returns an `Either` on failure
tryRoundFillet :: Double -> Solid -> Either WaterfallError Solid
tryRoundFillet r = tryRoundConditionalFillet (const . pure $ r)


-- | Version of `indexedConditionalChamfer` that returns an `Either` on failure
tryIndexedConditionalChamfer 
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Either WaterfallError Solid
tryIndexedConditionalChamfer radiusFunction solid = solidFromAcquireWithCatch (solidPaintMap solid) $ do
    s <- acquireSolid solid
    builder <- MakeChamfer.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdges radiusFunction (MakeChamfer.addEdgeWithDistance builder) explorer

    MakeShape.shape (upcast builder)

-- | Add chamfers of the given size to each edge of a solid, conditional on the endpoints of the edge, and the index of the edge.
-- 
-- This can be used to selectively chamfer a `Solid`.
--
-- In general, relying on the edge index is inelegant,
-- however, if you consider a Solid with a semicircular face, 
-- there's no way to select either the curved or the flat edge of the semicircle based on just the endpoints.
--
-- Being able to selectively chamfer based on edge index is an \"easy\" way to chamfer these shapes. 
indexedConditionalChamfer
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Solid
indexedConditionalChamfer radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamfer radiusFunction solid

-- | Version of `conditionalChamfer` that returns an `Either` on failure
tryConditionalChamfer 
    :: ((V3 Double, V3 Double) -> Maybe Double) 
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamfer f = tryIndexedConditionalChamfer (const f)

-- | Add chamfers with the given size to each edge of a solid, conditional on the endpoints of the edge.
-- 
-- This can be used to selectively chamfer a `Solid`.
conditionalChamfer :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
conditionalChamfer f = indexedConditionalChamfer (const f)

-- | Version of `chamfer` that returns an `Either` on failure
tryChamfer :: Double -> Solid -> Either WaterfallError Solid
tryChamfer d = tryConditionalChamfer (const . pure $ d)

-- | Add a chamfer with a given size to every edge of a solid
--
-- This is applied to both internal (concave) and external (convex) edges
chamfer :: Double -> Solid -> Solid
chamfer d = conditionalChamfer (const . pure $ d)

-- | Returns a value when the target of a lens on two points are close to one another.
-- 
-- This can be used in combination with `roundConditionalFillet`/`conditionalChamfer`.
--
-- Selecting only horizontal edges:
--
-- > roundConditionalFillet (whenNearlyEqual _z 2)
--
-- Selecting only vertical edges:
--
-- > roundConditionalFillet (whenNearlyEqual _xy 2)
whenNearlyEqual :: Epsilon a => Lens' point a -> r -> (point, point) -> Maybe r
whenNearlyEqual l res (s, e)
    | nearZero ((s ^. l) - (e ^. l))  = Just res
    | otherwise = Nothing                         
