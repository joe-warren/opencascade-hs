module Waterfall.Fillet
(
    roundFillet
) where

import Waterfall.Internal.Solid (Solid (..))
import qualified OpenCascade.BRepFilletAPI.MakeFillet as MakeFillet
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.TopExp.Explorer as Explorer 
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import Foreign.Ptr (Ptr)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (upcast, unsafeDowncast)

addEdgesToMakeFillet :: Double -> Ptr MakeFillet.MakeFillet -> Ptr Explorer.Explorer -> IO ()
addEdgesToMakeFillet radius builder explorer = 
    do
        isMore <- Explorer.more explorer
        when isMore $ do
            v <- unsafeDowncast =<< Explorer.value explorer 
            MakeFillet.addEdgeWithRadius builder radius v
            
            Explorer.next explorer
            addEdgesToMakeFillet radius builder explorer

roundFillet :: Double -> Solid -> Solid
roundFillet radius (Solid runSolid) = Solid $ do
    s <- runSolid 
    builder <- MakeFillet.fromShape s

    explorer <- Explorer.new s ShapeEnum.Edge
    liftIO $ addEdgesToMakeFillet radius builder explorer

    MakeShape.shape (upcast builder)

