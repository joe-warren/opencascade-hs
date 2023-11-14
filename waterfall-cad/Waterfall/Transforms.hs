module Waterfall.Transforms
( scale
, uScale
, rotate
, translate
) where
import qualified Waterfall.Solids as Solids
import qualified Linear.V3 as V3
import qualified OpenCascade.GP.Trsf as GP.Trsf
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Ax1 as GP.Ax1
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.Inheritance as Inheritance
import qualified OpenCascade.BRepBuilderAPI.Transform  as BRepBuilderAPI.Transform
import Control.Monad.IO.Class (liftIO)
import Data.Acquire
import Foreign.Ptr


fromTrsf :: Acquire (Ptr GP.Trsf) -> Solids.Solid -> Solids.Solid
fromTrsf mkTrsf (Solids.Solid run) = Solids.Solid $ do 
    solid <- run
    trsf <- mkTrsf 
    transformedShape <- BRepBuilderAPI.Transform.transform (Inheritance.upcast solid) trsf True 
    liftIO $ Inheritance.unsafeDowncast transformedShape

scale :: V3.V3 Double -> Solids.Solid -> Solids.Solid
scale (V3.V3 x y z) = fromTrsf $ do
    trsf <- GP.Trsf.new
    liftIO $ GP.Trsf.setValues trsf
            x 0 0 0
            0 y 0 0
            0 0 z 0
    return trsf 

-- uniformScale
uScale :: Double -> Solids.Solid -> Solids.Solid
uScale factor = fromTrsf $ do 
    trsf <- GP.Trsf.new
    o <- GP.origin
    liftIO $ GP.Trsf.setScale trsf o factor 
    return trsf

-- rotate by Axis + Angle (raw Quaternions are no fun)
rotate :: V3.V3 Double -> Double -> Solids.Solid -> Solids.Solid
rotate (V3.V3 ax ay az) angle = fromTrsf $ do
    trsf <- GP.Trsf.new
    o <- GP.origin
    dir <- GP.Dir.new ax ay az
    axis <- GP.Ax1.new o dir
    liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
    return trsf

translate :: V3.V3 Double -> Solids.Solid -> Solids.Solid
translate (V3.V3 x y z) = fromTrsf $ do 
    trsf <- GP.Trsf.new
    vec <- GP.Vec.new x y z
    liftIO $ GP.Trsf.setTranslation trsf vec
    return trsf