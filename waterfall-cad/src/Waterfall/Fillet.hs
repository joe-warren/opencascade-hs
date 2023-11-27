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
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.Geom.Curve as Geom.Curve
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt
import Foreign.Ptr (Ptr)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Linear.V3 (V3 (..))
import Data.Acquire 


addEdgesToMakeFillet :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Ptr MakeFillet.MakeFillet -> Ptr Explorer.Explorer -> IO ()
addEdgesToMakeFillet radiusFn builder explorer = go [] 0
    where go visited i = do
            isMore <- Explorer.more explorer
            when isMore $ do
                v <- unsafeDowncast =<< Explorer.value explorer
                hash <- TopoDS.Shape.hashCode (upcast v) (2^31)
                if(elem hash visited) 
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

roundIndexedConditionalFillet :: (Integer -> (V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundIndexedConditionalFillet radiusFunction (Solid runSolid) = Solid $ do
    s <- runSolid 
    builder <- MakeFillet.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdgesToMakeFillet radiusFunction builder explorer

    MakeShape.shape (upcast builder)

roundConditionalFillet :: ((V3 Double, V3 Double) -> Maybe Double) -> Solid -> Solid
roundConditionalFillet f = roundIndexedConditionalFillet (const f)

roundFillet :: Double -> Solid -> Solid
roundFillet r = roundConditionalFillet (const . pure $ r)
