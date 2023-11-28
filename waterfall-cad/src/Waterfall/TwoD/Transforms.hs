{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Waterfall.TwoD.Transforms
( Transformable2D
, rotate2D
, scale2D
, uScale2D
, translate2D
) where

import Waterfall.TwoD.Internal.Path2D (Path2D (..))
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
import Waterfall.TwoD.Internal.Shape (Shape(..))

class Transformable2D a where
    rotate2D :: Double -> a -> a
    scale2D :: V2 Double -> a -> a
    uScale2D :: Double -> a -> a
    translate2D :: V2 Double -> a -> a

fromTrsfPath :: Acquire (Ptr GP.Trsf) -> Path2D -> Path2D
fromTrsfPath mkTrsf (Path2D run) = Path2D $ do 
    path <- run
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.Transform.transform (upcast path) trsf True 

fromTrsfShape :: Acquire (Ptr GP.Trsf) -> Shape -> Shape
fromTrsfShape mkTrsf (Shape run) = Shape $ do 
    shape <- run
    trsf <- mkTrsf 
    BRepBuilderAPI.Transform.transform shape trsf True 

    
fromGTrsfPath :: Acquire (Ptr GP.GTrsf) -> Path2D -> Path2D
fromGTrsfPath mkTrsf (Path2D run) = Path2D $ do 
    path <- run
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.GTransform.gtransform (upcast path) trsf True 

fromGTrsfShape :: Acquire (Ptr GP.GTrsf) -> Shape -> Shape
fromGTrsfShape mkTrsf (Shape run) = Shape $ do 
    shape <- run
    trsf <- mkTrsf 
    BRepBuilderAPI.GTransform.gtransform shape trsf True 

rotateTrsf :: Double -> Acquire (Ptr GP.Trsf)
rotateTrsf angle = do
    trsf <- GP.Trsf.new
    o <- GP.origin
    dir <- GP.Dir.new 0 0 1
    axis <- GP.Ax1.new o dir
    liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
    return trsf

scaleGTrsf :: V2 Double -> Acquire (Ptr GP.GTrsf)
scaleGTrsf (V2 x y) = do
    trsf <- GP.GTrsf.new 
    liftIO $ do
        GP.GTrsf.setValue trsf 1 1 x
        GP.GTrsf.setValue trsf 2 2 y
        GP.GTrsf.setValue trsf 3 3 1
        GP.GTrsf.setForm trsf
        return trsf

uScaleTrsf :: Double -> Acquire (Ptr GP.Trsf) 
uScaleTrsf factor = do
    trsf <- GP.Trsf.new
    o <- GP.origin
    liftIO $ GP.Trsf.setScale trsf o factor 
    return trsf

translateTrsf :: V2 Double -> Acquire (Ptr GP.Trsf)
translateTrsf (V2 x y) = do 
    trsf <- GP.Trsf.new
    vec <- GP.Vec.new x y 0
    liftIO $ GP.Trsf.setTranslation trsf vec
    return trsf

instance Transformable2D Path2D where
    rotate2D :: Double -> Path2D -> Path2D
    rotate2D = fromTrsfPath . rotateTrsf  
    
    scale2D :: V2 Double -> Path2D -> Path2D
    scale2D = fromGTrsfPath . scaleGTrsf

    uScale2D :: Double -> Path2D -> Path2D
    uScale2D = fromTrsfPath . uScaleTrsf

    translate2D :: V2 Double -> Path2D -> Path2D
    translate2D = fromTrsfPath .translateTrsf


instance Transformable2D Shape where
    rotate2D :: Double -> Shape -> Shape
    rotate2D = fromTrsfShape . rotateTrsf  
    
    scale2D :: V2 Double -> Shape -> Shape
    scale2D = fromGTrsfShape . scaleGTrsf

    uScale2D :: Double -> Shape -> Shape
    uScale2D = fromTrsfShape . uScaleTrsf

    translate2D :: V2 Double -> Shape -> Shape
    translate2D = fromTrsfShape .translateTrsf

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