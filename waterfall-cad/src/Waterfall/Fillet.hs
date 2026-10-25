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
, roundFilletWithPaint
, roundConditionalFillet
, roundConditionalFilletWithPaint
, roundIndexedConditionalFillet
, roundIndexedConditionalFilletWithPaint
, tryRoundFillet
, tryRoundFilletWithPaint
, tryRoundConditionalFillet
, tryRoundConditionalFilletWithPaint
, tryRoundIndexedConditionalFillet
, tryRoundIndexedConditionalFilletWithPaint
-- * Chamfers
-- | Adds flat faces at a constant angle to the two faces either side of an edge.
, chamfer
, chamferWithPaint
, conditionalChamfer
, conditionalChamferWithPaint
, indexedConditionalChamfer
, indexedConditionalChamferWithPaint
, tryChamfer
, tryChamferWithPaint
, tryConditionalChamfer
, tryConditionalChamferWithPaint
, tryIndexedConditionalChamfer
, tryIndexedConditionalChamferWithPaint
-- * Utility Methods
, whenEndpointsNearlyEqual
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquireWithPaintMapWithCatch, makeShapeHistory)
import Waterfall.Internal.Edges (allEdges)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Path (pathEndpoints) 
import Waterfall.Error (WaterfallError)
import qualified OpenCascade.BRepFilletAPI.MakeFillet as MakeFillet
import qualified OpenCascade.BRepFilletAPI.MakeChamfer as MakeChamfer
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, SubTypeOf, unsafeDowncast)
import Linear.V3 (V3 (..))
import Linear.Epsilon ( nearZero, Epsilon)
import Control.Lens ((^.), Getter)
import Data.Either (fromRight)
import Waterfall.Paint (Paint)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Acquire (Acquire)
import Data.Maybe (catMaybes)
import Data.Foldable (find)
import qualified OpenCascade.BRepBuilderAPI as BRepBuilderAPI
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Waterfall.Internal.PaintHistory (remapWithBlendFunction)
import Waterfall.Internal.Path.Common (RawPath(ComplexRawPath))
import Waterfall.Internal.Finalizers (fromAcquirePtr)

edgeToPath :: Ptr TopoDS.Edge -> IO Path
edgeToPath edge = fmap (Path . ComplexRawPath) . fromAcquirePtr $ do
    builder <- MakeWire.new
    liftIO . MakeWire.addEdge builder 
        =<<  liftIO . unsafeDowncast 
        =<< TopoDS.Shape.copy (upcast edge)
    MakeWire.wire builder

getEdgesWithRadius :: (Integer -> Path -> Maybe Double) -> Ptr TopoDS.Shape -> Acquire [(Double, Ptr TopoDS.Edge)]
getEdgesWithRadius f s = do
    edges <- allEdges s
    fmap catMaybes . forM (zip [0..] edges) $ \(i, e) -> do
        path <- liftIO . edgeToPath $ e
        return $ (, e) <$> find (> 0) (f i path)

trySomeIndexedConditionalFilletWithPaint
    :: (SubTypeOf BRepBuilderAPI.MakeShape a)
    => (Ptr TopoDS.Shape -> Acquire (Ptr a))
    -> (Ptr a -> Double -> Ptr TopoDS.Edge -> IO ())
    -> (NonEmpty Paint -> Paint)
    -> (Integer -> Path -> Maybe Double)
    -> Solid
    -> Either WaterfallError Solid
trySomeIndexedConditionalFilletWithPaint makeBuilder addEdge paintFunction radiusFunction solid =
    solidFromAcquireWithPaintMapWithCatch $ do
        s <- acquireSolid solid
        builder <- makeBuilder s

        edgesWithRadius <- getEdgesWithRadius radiusFunction s

        liftIO $ forM_ edgesWithRadius (uncurry (addEdge builder))

        resultShape <- MakeShape.shape (upcast builder)

        newPaintMap <-
            remapWithBlendFunction
                paintFunction
                s
                (snd <$> edgesWithRadius)
                (makeShapeHistory . upcast $ builder)
                (solidPaintMap solid)

        pure (newPaintMap, resultShape)

-- | Version of `tryRoundIndexedConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
tryRoundIndexedConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge path to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFilletWithPaint =
    trySomeIndexedConditionalFilletWithPaint MakeFillet.fromShape MakeFillet.addEdgeWithRadius

-- | Version of `roundIndexedConditionalFillet` that returns an `Either` on failure
tryRoundIndexedConditionalFillet
    :: (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge path to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFillet =
    tryRoundIndexedConditionalFilletWithPaint NE.head

-- | Add rounds with the given radius to each edge of a solid, conditional on the path of the edge, and the index of the edge.
--
-- This can be used to selectively round\/fillet a `Solid`.
--
-- In general, relying on the edge index is inelegant,
--
-- In an earlier version of Waterfall-CAD, fillets were controlled by the edge *endpoints*, rather than the edge path
--  
-- If you consider a Solid with a semicircular face,
-- there's no way to select either the curved or the flat edge of the semicircle based on just the endpoints
--
-- Now that fillets can be constructed based on the whole `Path` of the edge, this argument holds less water.
--
-- But being able to selectively round\/fillet based on edge index is still an \"easy\" way to round\/fillet these shapes.
roundIndexedConditionalFillet
    :: (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge `Path` to the fillet radius
    -> Solid
    -> Solid
roundIndexedConditionalFillet radiusFunction solid = fromRight mempty $ tryRoundIndexedConditionalFillet radiusFunction solid


-- | Version of `roundIndexedConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
roundIndexedConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge `Path` to the fillet radius
    -> Solid
    -> Solid
roundIndexedConditionalFilletWithPaint paintFn radiusFunction solid =
    fromRight mempty $ tryRoundIndexedConditionalFilletWithPaint paintFn radiusFunction solid

-- | Version of `roundConditionalFillet` that returns an `Either` on failure
tryRoundConditionalFillet
    :: (Path -> Maybe Double) -- ^ Maps the edge `Path` to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundConditionalFillet f = tryRoundIndexedConditionalFillet (const f)

-- | Version of `tryRoundConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
tryRoundConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> (Path -> Maybe Double) -- ^ Maps the edge `Path` to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundConditionalFilletWithPaint paintFn f = tryRoundIndexedConditionalFilletWithPaint paintFn (const f)

-- | Add rounds with the given radius to each edge of a solid, conditional on the `Path` of the edge.
--
-- This can be used to selectively round\/fillet a `Solid`.
roundConditionalFillet
    :: (Path -> Maybe Double) -- ^ Maps the edge `Path` to the fillet radius
    -> Solid -> Solid
roundConditionalFillet f = roundIndexedConditionalFillet (const f)

-- | Version of `roundConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
roundConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> (Path -> Maybe Double) -- ^ Maps the edge `Path` to the fillet radius
    -> Solid -> Solid
roundConditionalFilletWithPaint paintFn f =
    roundIndexedConditionalFilletWithPaint paintFn (const f)

-- | Add a round with a given radius to every edge of a solid
--
-- Because this is applied to both internal (concave) and external (convex) edges, it may technically produce both Rounds and Fillets
roundFillet
    :: Double -- ^ The fillet radius
    -> Solid -> Solid
roundFillet r = roundConditionalFillet (const . pure $ r)


-- | Version of `roundFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
roundFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> Double -- ^ The fillet radius
    -> Solid -> Solid
roundFilletWithPaint paintFn r
    = roundConditionalFilletWithPaint paintFn (const . pure $ r)

-- | Version of `roundFillet` that returns an `Either` on failure
tryRoundFillet
    :: Double -- ^ The fillet radius
    -> Solid -> Either WaterfallError Solid
tryRoundFillet r = tryRoundConditionalFillet (const . pure $ r)

-- | Version of `tryRoundFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
tryRoundFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> Double -- ^ The fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundFilletWithPaint paintFn r =
    tryRoundConditionalFilletWithPaint paintFn (const . pure $ r)

-- | Version of `tryIndexedConditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
tryIndexedConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge `Path` to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryIndexedConditionalChamferWithPaint =
    trySomeIndexedConditionalFilletWithPaint MakeChamfer.fromShape MakeChamfer.addEdgeWithDistance

-- | Version of `indexedConditionalChamfer` that returns an `Either` on failure
tryIndexedConditionalChamfer
    :: (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge `Path` to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryIndexedConditionalChamfer =
    tryIndexedConditionalChamferWithPaint NE.head

-- | Add chamfers of the given size to each edge of a solid, conditional on the `Path` of the edge, and the index of the edge.
--
-- This can be used to selectively chamfer a `Solid`.
--
-- In general, relying on the edge index is inelegant,
-- however, if you consider a Solid with a semicircular face,
-- there's no way to select either the curved or the flat edge of the semicircle based on just the `Path`.
--
-- Being able to selectively chamfer based on edge index is an \"easy\" way to chamfer these shapes.
indexedConditionalChamfer
    :: (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge `Path` to the chamfer size
    -> Solid
    -> Solid
indexedConditionalChamfer radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamfer radiusFunction solid

-- | Version of `indexedConditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
indexedConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> (Integer -> Path -> Maybe Double) -- ^ Maps the edge index and the edge `Path` to the chamfer size
    -> Solid
    -> Solid
indexedConditionalChamferWithPaint paintFn radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamferWithPaint paintFn radiusFunction solid

-- | Version of `conditionalChamfer` that returns an `Either` on failure
tryConditionalChamfer
    :: (Path -> Maybe Double) -- ^ Maps the edge `Path` to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamfer f = tryIndexedConditionalChamfer (const f)


-- | Version of `tryConditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
tryConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> (Path -> Maybe Double)  -- ^ Maps the edge `Path` to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamferWithPaint paintFn f = tryIndexedConditionalChamferWithPaint paintFn (const f)

-- | Add chamfers with the given size to each edge of a solid, conditional on the `Path` of the edge.
--
-- This can be used to selectively chamfer a `Solid`.
conditionalChamfer
    :: (Path -> Maybe Double)  -- ^ Maps the edge `Path` to the chamfer size
    -> Solid
    -> Solid
conditionalChamfer f = indexedConditionalChamfer (const f)


-- | Version of `conditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
conditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> (Path -> Maybe Double) -- ^ Maps the edge `Path` to the chamfer size
    -> Solid -> Solid
conditionalChamferWithPaint paintFn f = indexedConditionalChamferWithPaint paintFn (const f)

-- | Version of `chamfer` that returns an `Either` on failure
tryChamfer :: Double -> Solid -> Either WaterfallError Solid
tryChamfer d = tryConditionalChamfer (const . pure $ d)


-- | Version of `tryChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
tryChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> Double -- ^ The chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryChamferWithPaint paintFn d = tryConditionalChamferWithPaint paintFn (const . pure $ d)

-- | Add a chamfer with a given size to every edge of a solid
--
-- This is applied to both internal (concave) and external (convex) edges
chamfer
    :: Double -- ^ The chamfer size
    -> Solid -> Solid
chamfer d = conditionalChamfer (const . pure $ d)

-- | Version of `chamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
chamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> Double  -- ^ The chamfer size
    -> Solid -> Solid
chamferWithPaint paintFunction d =
    conditionalChamferWithPaint paintFunction (const . pure $ d )

-- | Returns a value when the target of a lens, when applied to the edge's `Path`'s endpoints are close to one another.
--
-- This can be used in combination with `roundConditionalFillet`/`conditionalChamfer`.
--
-- Selecting only horizontal edges:
--
-- > roundConditionalFillet (whenEndpointsNearlyEqual _z 2)
--
-- Selecting only vertical edges:
--
-- > roundConditionalFillet (whenEndpointsNearlyEqual _xy 2)
whenEndpointsNearlyEqual :: (Epsilon v) => Getter (V3 Double) v -> r -> Path -> Maybe r
whenEndpointsNearlyEqual l res path = 
    case pathEndpoints path of 
        Nothing -> Nothing
        Just (s, e) | nearZero ((s ^. l) - (e ^. l)) -> Just res
                    | otherwise -> Nothing


