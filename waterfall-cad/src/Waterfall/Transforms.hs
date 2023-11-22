{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Waterfall.Transforms
( Transformable
, scale
, uScale
, rotate
, translate
) where
import Waterfall.Internal.Solid (Solid(..))
import Linear.V3 (V3 (..))
import Linear ((*^))
import qualified Linear.V3 as V3
import qualified Linear.Quaternion as Quaternion
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

class Transformable a where
    scale :: V3 Double -> a -> a
    -- Uniform Scale
    uScale :: Double -> a -> a
    -- rotate by Axis + Angle (raw Quaternions are no fun)
    rotate :: V3 Double -> Double -> a -> a
    translate :: V3 Double -> a -> a


fromTrsf :: Acquire (Ptr GP.Trsf) -> Solid -> Solid
fromTrsf mkTrsf (Solid run) = Solid $ do 
    solid <- run
    trsf <- mkTrsf 
    BRepBuilderAPI.Transform.transform solid trsf True 

instance Transformable Solid where
    scale :: V3 Double -> Solid -> Solid
    scale (V3 x y z) (Solid run) = Solid $ do 
        solid <- run
        trsf <- GP.GTrsf.new 
        liftIO $ do
            GP.GTrsf.setValue trsf 1 1 x
            GP.GTrsf.setValue trsf 2 2 y
            GP.GTrsf.setValue trsf 3 3 z
            GP.GTrsf.setForm trsf
        BRepBuilderAPI.GTransform.gtransform solid trsf True 

    uScale :: Double -> Solid -> Solid
    uScale factor = fromTrsf $ do 
        trsf <- GP.Trsf.new
        o <- GP.origin
        liftIO $ GP.Trsf.setScale trsf o factor 
        return trsf

    rotate :: V3 Double -> Double -> Solid -> Solid
    rotate (V3 ax ay az) angle = fromTrsf $ do
        trsf <- GP.Trsf.new
        o <- GP.origin
        dir <- GP.Dir.new ax ay az
        axis <- GP.Ax1.new o dir
        liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
        return trsf

    translate :: V3 Double -> Solid -> Solid
    translate (V3 x y z) = fromTrsf $ do 
        trsf <- GP.Trsf.new
        vec <- GP.Vec.new x y z
        liftIO $ GP.Trsf.setTranslation trsf vec
        return trsf

        
instance Transformable (V3 Double) where
    scale :: V3 Double -> V3 Double  -> V3 Double
    scale = (*)

    -- Uniform Scale
    uScale :: Double -> V3 Double -> V3 Double
    uScale = (*^)

    rotate :: V3 Double -> Double -> V3 Double -> V3 Double 
    rotate axis angle = Quaternion.rotate (Quaternion.axisAngle axis angle)

    translate :: V3 Double -> V3 Double -> V3 Double 
    translate = (+)

