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
, whenNearlyEqual
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, PaintMap (..), solidFromAcquireWithPaintMapWithCatch, materialiseFacePaints, makeShapeHistory)
import Waterfall.Internal.Edges (edgeEndpoints, allEdges)
import Waterfall.Error (WaterfallError)
import qualified OpenCascade.BRepFilletAPI.MakeFillet as MakeFillet
import qualified OpenCascade.BRepFilletAPI.MakeChamfer as MakeChamfer
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, SubTypeOf)
import Linear.V3 (V3 (..))
import Linear.Epsilon (Epsilon, nearZero)
import Control.Lens (Lens', (^.))
import Data.Either (fromRight)
import Waterfall.Paint (Paint)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Acquire (Acquire)
import Data.Maybe (catMaybes)
import Data.Foldable (find)
import qualified OpenCascade.BRepBuilderAPI.MakeShape as BRepBuilderAPI
import Waterfall.Internal.PaintHistory (remapWithBlendFunction)

getEdgesWithRadius :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Ptr TopoDS.Shape -> Acquire [(Double, Ptr TopoDS.Edge)]
getEdgesWithRadius f s = do
    edges <- allEdges s
    fmap catMaybes . forM (zip [0..] edges) $ \(i, e) -> do
        endpoints <- liftIO $ edgeEndpoints e
        return $ (, e) <$> find (> 0) (f i endpoints)

trySomeIndexedConditionalFilletWithPaint
    :: (SubTypeOf BRepBuilderAPI.MakeShape a)
    => (Ptr TopoDS.Shape -> Acquire (Ptr a))
    -> (Ptr a -> Double -> Ptr TopoDS.Edge -> IO ())
    -> (NonEmpty Paint -> Paint)
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double)
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
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFilletWithPaint =
    trySomeIndexedConditionalFilletWithPaint MakeFillet.fromShape MakeFillet.addEdgeWithRadius

-- | Version of `roundIndexedConditionalFillet` that returns an `Either` on failure
tryRoundIndexedConditionalFillet
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFillet =
    tryRoundIndexedConditionalFilletWithPaint NE.head

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
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the fillet radius
    -> Solid
    -> Solid
roundIndexedConditionalFillet radiusFunction solid = fromRight mempty $ tryRoundIndexedConditionalFillet radiusFunction solid


-- | Version of `roundIndexedConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
roundIndexedConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the fillet radius
    -> Solid
    -> Solid
roundIndexedConditionalFilletWithPaint paintFn radiusFunction solid =
    fromRight mempty $ tryRoundIndexedConditionalFilletWithPaint paintFn radiusFunction solid

-- | Version of `roundConditionalFillet` that returns an `Either` on failure
tryRoundConditionalFillet
    :: ((V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge endpoints to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundConditionalFillet f = tryRoundIndexedConditionalFillet (const f)

-- | Version of `tryRoundConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
tryRoundConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> ((V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge endpoints to the fillet radius
    -> Solid
    -> Either WaterfallError Solid
tryRoundConditionalFilletWithPaint paintFn f = tryRoundIndexedConditionalFilletWithPaint paintFn (const f)

-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge.
--
-- This can be used to selectively round\/fillet a `Solid`.
roundConditionalFillet
    :: ((V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge endpoints to the fillet radius
    -> Solid -> Solid
roundConditionalFillet f = roundIndexedConditionalFillet (const f)

-- | Version of `roundConditionalFillet`
--
-- Exposes how to assign `Paint` to the fillet surface
roundConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a fillet surface to the `Paint` used for the surface
    -> ((V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge endpoints to the fillet radius
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
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryIndexedConditionalChamferWithPaint =
    trySomeIndexedConditionalFilletWithPaint MakeChamfer.fromShape MakeChamfer.addEdgeWithDistance

-- | Version of `indexedConditionalChamfer` that returns an `Either` on failure
tryIndexedConditionalChamfer
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryIndexedConditionalChamfer =
    tryIndexedConditionalChamferWithPaint NE.head

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
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the chamfer size
    -> Solid
    -> Solid
indexedConditionalChamfer radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamfer radiusFunction solid

-- | Version of `indexedConditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
indexedConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge index and the edge endpoints to the chamfer size
    -> Solid
    -> Solid
indexedConditionalChamferWithPaint paintFn radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamferWithPaint paintFn radiusFunction solid

-- | Version of `conditionalChamfer` that returns an `Either` on failure
tryConditionalChamfer
    :: ((V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge endpoints to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamfer f = tryIndexedConditionalChamfer (const f)


-- | Version of `tryConditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
tryConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> ((V3 Double, V3 Double) -> Maybe Double)  -- ^ Maps the edge endpoints to the chamfer size
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamferWithPaint paintFn f = tryIndexedConditionalChamferWithPaint paintFn (const f)

-- | Add chamfers with the given size to each edge of a solid, conditional on the endpoints of the edge.
--
-- This can be used to selectively chamfer a `Solid`.
conditionalChamfer
    :: ((V3 Double, V3 Double) -> Maybe Double)  -- ^ Maps the edge endpoints to the chamfer size
    -> Solid
    -> Solid
conditionalChamfer f = indexedConditionalChamfer (const f)


-- | Version of `conditionalChamfer`
--
-- Exposes how to assign `Paint` to the chamfer surface
conditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint) -- ^ Maps the `Paint` on the neighbouring faces of a chamfer surface to the `Paint` used for the surface
    -> ((V3 Double, V3 Double) -> Maybe Double) -- ^ Maps the edge endpoints to the chamfer size
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
