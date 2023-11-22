{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Waterfall.TwoD.Transforms
( Transformable2D
, rotate2D
, scale2D
, uScale2D
, translate2D
) where

import Waterfall.TwoD.Internal.Path (Path (..))
import Linear.V2 (V2 (..))
import Linear ((*^))
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

class Transformable2D a where
    rotate2D :: Double -> a -> a
    scale2D :: V2 Double -> a -> a
    uScale2D :: Double -> a -> a
    translate2D :: V2 Double -> a -> a

fromTrsf :: Acquire (Ptr GP.Trsf) -> Path -> Path
fromTrsf mkTrsf (Path run) = Path $ do 
    path <- run
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.Transform.transform (upcast path) trsf True 

instance Transformable2D Path where
    rotate2D :: Double -> Path -> Path
    rotate2D angle = fromTrsf $ do
        trsf <- GP.Trsf.new
        o <- GP.origin
        dir <- GP.Dir.new 0 0 1
        axis <- GP.Ax1.new o dir
        liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
        return trsf

    scale2D :: V2 Double -> Path -> Path
    scale2D (V2 x y) (Path run) = Path $ do 
        path <- run
        trsf <- GP.GTrsf.new 
        liftIO $ do
            GP.GTrsf.setValue trsf 1 1 x
            GP.GTrsf.setValue trsf 2 2 y
            GP.GTrsf.setValue trsf 3 3 1
            GP.GTrsf.setForm trsf
        (liftIO .unsafeDowncast) =<< BRepBuilderAPI.GTransform.gtransform (upcast path) trsf True 

    uScale2D :: Double -> Path -> Path
    uScale2D factor = fromTrsf $ do 
        trsf <- GP.Trsf.new
        o <- GP.origin
        liftIO $ GP.Trsf.setScale trsf o factor 
        return trsf

    translate2D :: V2 Double -> Path -> Path
    translate2D (V2 x y) = fromTrsf $ do 
        trsf <- GP.Trsf.new
        vec <- GP.Vec.new x y 0
        liftIO $ GP.Trsf.setTranslation trsf vec
        return trsf

instance Transformable2D (V2 Double) where
    scale2D :: V2 Double -> V2 Double  -> V2 Double
    scale2D = (*)

    -- Uniform Scale
    uScale2D :: Double -> V2 Double -> V2 Double
    uScale2D = (*^)

    rotate2D :: Double -> V2 Double -> V2 Double 
    rotate2D angle (V2 x y) = 
        let c = cos angle 
            s = sin angle     
         in V2 (x * c - y * s) (x * s + y * c)

    translate2D :: V2 Double -> V2 Double -> V2 Double 
    translate2D = (+)