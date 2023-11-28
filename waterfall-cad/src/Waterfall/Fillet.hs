module Waterfall.Fillet
( roundFillet
, roundConditionalFillet
, roundIndexedConditionalFillet
) where

import Waterfall.Internal.Solid (Solid (..))
import Waterfall.Internal.Edges (edgeEndpoints)
import qualified OpenCascade.BRepFilletAPI.MakeFillet as MakeFillet
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopExp.Explorer as Explorer 
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Foreign.Ptr (Ptr)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Linear.V3 (V3 (..))


addEdgesToMakeFillet :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Ptr MakeFillet.MakeFillet -> Ptr Explorer.Explorer -> IO ()
addEdgesToMakeFillet radiusFn builder explorer = go [] 0
    where go visited i = do
            isMore <- Explorer.more explorer
            when isMore $ do
                v <- unsafeDowncast =<< Explorer.value explorer
                hash <- TopoDS.Shape.hashCode (upcast v) (2^(31 :: Int))
                if hash `elem` visited
                    then do
                        Explorer.next explorer
                        go visited i
                    else do
                        endpoints <- edgeEndpoints v
                        case radiusFn i endpoints of 
                            Just r | r > 0 -> MakeFillet.addEdgeWithRadius builder r v
                            _ -> pure ()
                        Explorer.next explorer
                        go (hash:visited) (i + 1) 

-- | Add rounds with the given radius to each edge of a solid, conditional on the endpoints of the edge, and the index of the edge.
-- 
-- This can be used to selectively round\/fillet a `Solid`.
--
-- In general, relying on the edge index is inelegant,
-- however, if you consider a Solid with a semicircular face, 
-- there's no way to select either the curved or the flat edge of the semicircle based on just the endpoints.
--
-- Being able to selectively round\/fillet based on edge index is an \"easy\" way to round\/fillet these shapes. 
roundIndexedConditionalFillet :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundIndexedConditionalFillet radiusFunction (Solid run) = Solid $ do
    s <- run
    builder <- MakeFillet.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdgesToMakeFillet radiusFunction builder explorer

    MakeShape.shape (upcast builder)

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
