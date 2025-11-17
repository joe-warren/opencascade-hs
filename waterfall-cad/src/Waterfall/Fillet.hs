{-# LANGUAGE RankNTypes #-}
module Waterfall.Fillet
( 
-- * \"Unsafe\" Functions
-- | These functions may raise an exception if they're unable to construct a fillet

-- ** Rounds
  roundFillet
, roundConditionalFillet
, roundIndexedConditionalFillet
-- ** Chamfers
, chamfer
, conditionalChamfer
, indexedConditionalChamfer
-- * \"Safe\" functions
--
--  | These functions return `Nothing`, rather than throwing, if they're unable to construct a fillet

-- ** Rounds
, safeRoundFillet
, safeRoundConditionalFillet
, safeRoundIndexedConditionalFillet
-- ** Chamfers
, safeChamfer
, safeConditionalChamfer
, safeIndexedConditionalChamfer
-- * Utility Methods
, whenNearlyEqual
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquireT)
import Waterfall.Internal.Edges (edgeEndpoints)
import qualified OpenCascade.BRepFilletAPI.MakeFillet as MakeFillet
import qualified OpenCascade.BRepFilletAPI.MakeChamfer as MakeChamfer
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopExp.Explorer as Explorer 
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopTools.ShapeMapHasher as TopTools.ShapeMapHasher
import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Linear.V3 (V3 (..))
import Linear.Epsilon 
import Control.Lens (Lens', (^.))
import Data.Maybe (fromMaybe)

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

filletError :: a
filletError = error "Unable to compute Fillet"

-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge, and the index of the edge.
-- 
-- This can be used to selectively round\/fillet a `Solid`.
--
-- In general, relying on the edge index is inelegant,
-- however, if you consider a Solid with a semicircular face, 
-- there's no way to select either the curved or the flat edge of the semicircle based on just the endpoints.
--
-- Being able to selectively round\/fillet based on edge index is an \"easy\" way to round\/fillet these shapes. 
--
-- Throws if the geometry kernel is unable to construct a Fillet
roundIndexedConditionalFillet :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundIndexedConditionalFillet radiusFunction solid = fromMaybe filletError $ safeRoundIndexedConditionalFillet radiusFunction solid

-- | Version of `roundIndexedConditionalFillet` that returns a `Maybe` rather than throwing
safeRoundIndexedConditionalFillet :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Solid -> Maybe Solid
safeRoundIndexedConditionalFillet radiusFunction solid = solidFromAcquireT $ do
    s <- acquireSolid solid
    builder <- MakeFillet.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdges radiusFunction (MakeFillet.addEdgeWithRadius builder) explorer
    done <- liftIO $ MakeShape.isDone (upcast builder)
    if done
        then Just <$> MakeShape.shape (upcast builder)
        else pure Nothing


-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge.
-- 
-- This can be used to selectively round\/fillet a `Solid`.
--
-- Throws if the geometry kernel is unable to construct a Fillet
roundConditionalFillet :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundConditionalFillet radiusFunction solid = fromMaybe filletError $ safeRoundConditionalFillet radiusFunction solid

-- | Version of `roundConditionalFillet` that returns a `Maybe` rather than throwing
safeRoundConditionalFillet :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Maybe Solid
safeRoundConditionalFillet f = safeRoundIndexedConditionalFillet (const f)

-- | Add a round with a given radius to every edge of a solid
--
-- Because this is applied to both internal (concave) and external (convex) edges, it may technically produce both Rounds and Fillets
--
-- Throws if the geometry kernel is unable to construct the Fillet
roundFillet :: Double -> Solid -> Solid
roundFillet r solid = fromMaybe filletError $ safeRoundFillet r solid

-- | Version of `roundFillet` that returns a `Maybe` rather than throwing
safeRoundFillet :: Double -> Solid -> Maybe Solid
safeRoundFillet r = safeRoundConditionalFillet (const . pure $ r)

chamferError :: a
chamferError = error "Unable to compute Chamfer"

-- | Add chamfers of the given size to each edge of a solid, conditional on the endpoints of the edge, and the index of the edge.
-- 
-- This can be used to selectively chamfer a `Solid`.
--
-- In general, relying on the edge index is inelegant,
-- however, if you consider a Solid with a semicircular face, 
-- there's no way to select either the curved or the flat edge of the semicircle based on just the endpoints.
--
-- Being able to selectively chamfer based on edge index is an \"easy\" way to chamfer these shapes. 
--
-- Throws if the geometry kernel is unable to construct the Chamfer
indexedConditionalChamfer :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
indexedConditionalChamfer radiusFunction solid = fromMaybe chamferError $ safeIndexedConditionalChamfer radiusFunction solid

-- | Version of `indexedConditionalChamfer` that returns a `Maybe` rather than throwing
safeIndexedConditionalChamfer :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Solid -> Maybe Solid
safeIndexedConditionalChamfer radiusFunction solid = solidFromAcquireT $ do
    s <- acquireSolid solid
    builder <- MakeChamfer.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdges radiusFunction (MakeChamfer.addEdgeWithDistance builder) explorer
    done <- liftIO $ MakeShape.isDone (upcast builder)
    if done
        then Just <$> MakeShape.shape (upcast builder)
        else pure Nothing

-- | Add chamfers with the given size to each edge of a solid, conditional on the endpoints of the edge.
-- 
-- This can be used to selectively chamfer a `Solid`.
--
conditionalChamfer :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
conditionalChamfer f = fromMaybe chamferError . safeConditionalChamfer f

-- | Version of `conditionalChamfer` that returns a `Maybe` rather than throwing
safeConditionalChamfer :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Maybe Solid
safeConditionalChamfer f = safeIndexedConditionalChamfer (const f)

-- | Add a round with a given radius to every edge of a solid
--
-- This is applied to both internal (concave) and external (convex) edges
--
--
chamfer :: Double -> Solid -> Solid 
chamfer d = fromMaybe chamferError . safeChamfer d


-- | Version of `chamfer` that returns a `Maybe` rather than throwing
safeChamfer :: Double -> Solid -> Maybe Solid
safeChamfer d = safeConditionalChamfer (const . pure $ d)

-- | Returns a value when the target of a lens on a two points are close to one another.
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
