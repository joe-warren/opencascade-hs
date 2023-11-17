module Waterfall.Transforms
( scale
, uScale
, rotate
, translate
) where
import Waterfall.Internal.Solid (Solid(..))
import qualified Linear.V3 as V3
import qualified OpenCascade.GP.Trsf as GP.Trsf
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.GTrsf as GP.GTrsf
import qualified OpenCascade.GP.Ax1 as GP.Ax1
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.BRepBuilderAPI.Transform  as BRepBuilderAPI.Transform
import qualified OpenCascade.BRepBuilderAPI.GTransform  as BRepBuilderAPI.GTransform
import Control.Monad.IO.Class (liftIO)
import Data.Acquire
import Foreign.Ptr


fromTrsf :: Acquire (Ptr GP.Trsf) -> Solid -> Solid
fromTrsf mkTrsf (Solid run) = Solid $ do 
    solid <- run
    trsf <- mkTrsf 
    BRepBuilderAPI.Transform.transform solid trsf True 

scale :: V3.V3 Double -> Solid -> Solid
scale (V3.V3 x y z) (Solid run) = Solid $ do 
    solid <- run
    trsf <- GP.GTrsf.new 
    liftIO $ do
        GP.GTrsf.setValue trsf 1 1 x
        GP.GTrsf.setValue trsf 2 2 y
        GP.GTrsf.setValue trsf 3 3 z
        GP.GTrsf.setForm trsf
    BRepBuilderAPI.GTransform.gtransform solid trsf True 

-- uniformScale
uScale :: Double -> Solid -> Solid
uScale factor = fromTrsf $ do 
    trsf <- GP.Trsf.new
    o <- GP.origin
    liftIO $ GP.Trsf.setScale trsf o factor 
    return trsf

-- rotate by Axis + Angle (raw Quaternions are no fun)
rotate :: V3.V3 Double -> Double -> Solid -> Solid
rotate (V3.V3 ax ay az) angle = fromTrsf $ do
    trsf <- GP.Trsf.new
    o <- GP.origin
    dir <- GP.Dir.new ax ay az
    axis <- GP.Ax1.new o dir
    liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
    return trsf

translate :: V3.V3 Double -> Solid -> Solid
translate (V3.V3 x y z) = fromTrsf $ do 
    trsf <- GP.Trsf.new
    vec <- GP.Vec.new x y z
    liftIO $ GP.Trsf.setTranslation trsf vec
    return trsf