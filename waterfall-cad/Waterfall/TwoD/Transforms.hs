module Waterfall.TwoD.Transforms
(rotate2D
) where

import Waterfall.TwoD.Internal.Path (Path (..))
import qualified OpenCascade.GP.Trsf as GP.Trsf
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.GTrsf as GP.GTrsf
import qualified OpenCascade.GP.Ax1 as GP.Ax1
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.BRepBuilderAPI.Transform  as BRepBuilderAPI.Transform
import qualified OpenCascade.BRepBuilderAPI.GTransform  as BRepBuilderAPI.GTransform
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire
import Foreign.Ptr

fromTrsf :: Acquire (Ptr GP.Trsf) -> Path -> Path
fromTrsf mkTrsf (Path run) = Path $ do 
    path <- run
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.Transform.transform (upcast path) trsf True 

rotate2D :: Double -> Path -> Path
rotate2D angle = fromTrsf $ do
    trsf <- GP.Trsf.new
    o <- GP.origin
    dir <- GP.Dir.new 0 0 1
    axis <- GP.Ax1.new o dir
    liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
    return trsf