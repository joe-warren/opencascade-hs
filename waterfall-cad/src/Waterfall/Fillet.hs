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

tryRoundIndexedConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint)
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Either WaterfallError Solid
tryRoundIndexedConditionalFilletWithPaint = 
    trySomeIndexedConditionalFilletWithPaint MakeFillet.fromShape MakeFillet.addEdgeWithRadius
        
-- | Version of `roundIndexedConditionalFillet` that returns an `Either` on failure
tryRoundIndexedConditionalFillet
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
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
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Solid
roundIndexedConditionalFillet radiusFunction solid = fromRight mempty $ tryRoundIndexedConditionalFillet radiusFunction solid


roundIndexedConditionalFilletWithPaint 
    :: (NonEmpty Paint -> Paint)
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Solid
roundIndexedConditionalFilletWithPaint paintFn radiusFunction solid = 
    fromRight mempty $ tryRoundIndexedConditionalFilletWithPaint paintFn radiusFunction solid

-- | Version of `roundConditionalFillet` that returns an `Either` on failure
tryRoundConditionalFillet 
    :: ((V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Either WaterfallError Solid
tryRoundConditionalFillet f = tryRoundIndexedConditionalFillet (const f)

tryRoundConditionalFilletWithPaint
    :: (NonEmpty Paint -> Paint)
    -> ((V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Either WaterfallError Solid
tryRoundConditionalFilletWithPaint paintFn f = tryRoundIndexedConditionalFilletWithPaint paintFn (const f)

-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge.
-- 
-- This can be used to selectively round\/fillet a `Solid`.
roundConditionalFillet :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundConditionalFillet f = roundIndexedConditionalFillet (const f)

roundConditionalFilletWithPaint 
    :: (NonEmpty Paint -> Paint) 
    -> ((V3 Double, V3 Double) -> Maybe Double) 
    -> Solid -> Solid
roundConditionalFilletWithPaint paintFn f =
    roundIndexedConditionalFilletWithPaint paintFn (const f)

-- | Add a round with a given radius to every edge of a solid
--
-- Because this is applied to both internal (concave) and external (convex) edges, it may technically produce both Rounds and Fillets
roundFillet :: Double -> Solid -> Solid
roundFillet r = roundConditionalFillet (const . pure $ r)

roundFilletWithPaint 
    :: (NonEmpty Paint -> Paint) 
    -> Double
    -> Solid -> Solid
roundFilletWithPaint paintFn r 
    = roundConditionalFilletWithPaint paintFn (const . pure $ r)

-- | Version of `roundFillet` that returns an `Either` on failure
tryRoundFillet :: Double -> Solid -> Either WaterfallError Solid
tryRoundFillet r = tryRoundConditionalFillet (const . pure $ r)

tryRoundFilletWithPaint 
    :: (NonEmpty Paint -> Paint)
    -> Double
    -> Solid
    -> Either WaterfallError Solid
tryRoundFilletWithPaint paintFn r =
    tryRoundConditionalFilletWithPaint paintFn (const . pure $ r) 

tryIndexedConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint)
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid
    -> Either WaterfallError Solid
tryIndexedConditionalChamferWithPaint = 
    trySomeIndexedConditionalFilletWithPaint MakeChamfer.fromShape MakeChamfer.addEdgeWithDistance

-- | Version of `indexedConditionalChamfer` that returns an `Either` on failure
tryIndexedConditionalChamfer 
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
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
    :: (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Solid
indexedConditionalChamfer radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamfer radiusFunction solid

indexedConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint)
    -> (Integer -> (V3 Double, V3 Double) -> Maybe Double)
    -> Solid 
    -> Solid
indexedConditionalChamferWithPaint paintFn radiusFunction solid =
    fromRight mempty $ tryIndexedConditionalChamferWithPaint paintFn radiusFunction solid

-- | Version of `conditionalChamfer` that returns an `Either` on failure
tryConditionalChamfer 
    :: ((V3 Double, V3 Double) -> Maybe Double) 
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamfer f = tryIndexedConditionalChamfer (const f)


tryConditionalChamferWithPaint
    :: (NonEmpty Paint -> Paint)
    -> ((V3 Double, V3 Double) -> Maybe Double) 
    -> Solid
    -> Either WaterfallError Solid
tryConditionalChamferWithPaint paintFn f = tryIndexedConditionalChamferWithPaint paintFn (const f)

-- | Add chamfers with the given size to each edge of a solid, conditional on the endpoints of the edge.
-- 
-- This can be used to selectively chamfer a `Solid`.
conditionalChamfer :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
conditionalChamfer f = indexedConditionalChamfer (const f)


conditionalChamferWithPaint 
    :: (NonEmpty Paint -> Paint)
    -> ((V3 Double, V3 Double) 
    -> Maybe Double) 
    -> Solid -> Solid
conditionalChamferWithPaint paintFn f = indexedConditionalChamferWithPaint paintFn (const f)

-- | Version of `chamfer` that returns an `Either` on failure
tryChamfer :: Double -> Solid -> Either WaterfallError Solid
tryChamfer d = tryConditionalChamfer (const . pure $ d)

tryChamferWithPaint 
    :: (NonEmpty Paint -> Paint) 
    -> Double 
    -> Solid 
    -> Either WaterfallError Solid
tryChamferWithPaint paintFn d = tryConditionalChamferWithPaint paintFn (const . pure $ d)

-- | Add a chamfer with a given size to every edge of a solid
--
-- This is applied to both internal (concave) and external (convex) edges
chamfer :: Double -> Solid -> Solid
chamfer d = conditionalChamfer (const . pure $ d)

chamferWithPaint 
    :: (NonEmpty Paint -> Paint)
    -> Double -> Solid -> Solid
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
