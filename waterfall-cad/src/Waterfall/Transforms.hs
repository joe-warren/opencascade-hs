{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Waterfall.Transforms
( Transformable
, scale
, uScale
, rotate
, translate
, mirror
) where
import Waterfall.Internal.Solid (Solid(..))
import Linear.V3 (V3 (..))
import Linear ((*^), normalize, dot )
import qualified Linear.Quaternion as Quaternion
import qualified OpenCascade.GP.Trsf as GP.Trsf
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.GTrsf as GP.GTrsf
import qualified OpenCascade.GP.Ax1 as GP.Ax1
import qualified OpenCascade.GP.Ax2 as GP.Ax2
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.BRepBuilderAPI.Transform  as BRepBuilderAPI.Transform
import qualified OpenCascade.BRepBuilderAPI.GTransform  as BRepBuilderAPI.GTransform
import Control.Monad.IO.Class (liftIO)
import Data.Acquire
import Foreign.Ptr
import Waterfall.Internal.Path (Path(..))
import OpenCascade.Inheritance (upcast, unsafeDowncast)

-- | Typeclass for objects that can be manipulated in 3D space
class Transformable a where
    -- | Scale by different amounts along the x, y and z axes
    scale :: V3 Double -> a -> a
    -- Uniform Scale
    -- | Scale uniformally along all axes
    uScale :: Double -> a -> a
    -- | Rotate by Axis and Angle (in radians)
    rotate :: V3 Double -> Double -> a -> a
    -- | Translate by a vector in 3D space
    translate :: V3 Double -> a -> a
    -- | Mirror in the plane, which passes through the origin, tangent to the specified vector
    mirror :: V3 Double -> a -> a


fromTrsfSolid :: Acquire (Ptr GP.Trsf) -> Solid -> Solid
fromTrsfSolid mkTrsf (Solid run) = Solid $ do 
    solid <- run
    trsf <- mkTrsf 
    BRepBuilderAPI.Transform.transform solid trsf True 


fromGTrsfSolid :: Acquire (Ptr GP.GTrsf) -> Solid -> Solid
fromGTrsfSolid mkTrsf (Solid run) = Solid $ do 
    solid <- run
    trsf <- mkTrsf 
    BRepBuilderAPI.GTransform.gtransform solid trsf True 


fromTrsfPath :: Acquire (Ptr GP.Trsf) -> Path -> Path
fromTrsfPath mkTrsf (Path run) = Path $ do 
    path <- run
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.Transform.transform (upcast path) trsf True 

fromGTrsfPath :: Acquire (Ptr GP.GTrsf) -> Path -> Path
fromGTrsfPath mkTrsf (Path run) = Path $ do 
    path <- run
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.GTransform.gtransform (upcast path) trsf True 

scaleTrsf :: V3 Double -> Acquire (Ptr GP.GTrsf)
scaleTrsf (V3 x y z ) = do
    trsf <- GP.GTrsf.new 
    liftIO $ do
        GP.GTrsf.setValue trsf 1 1 x
        GP.GTrsf.setValue trsf 2 2 y
        GP.GTrsf.setValue trsf 3 3 z
        GP.GTrsf.setForm trsf
        return trsf

uScaleTrsf :: Double -> Acquire (Ptr GP.Trsf)
uScaleTrsf factor = do
    trsf <- GP.Trsf.new
    o <- GP.origin
    liftIO $ GP.Trsf.setScale trsf o factor 
    return trsf

rotateTrsf :: V3 Double -> Double -> Acquire (Ptr GP.Trsf)
rotateTrsf (V3 ax ay az) angle = do
    trsf <- GP.Trsf.new
    o <- GP.origin
    dir <- GP.Dir.new ax ay az
    axis <- GP.Ax1.new o dir
    liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
    return trsf

translateTrsf :: V3 Double -> Acquire (Ptr GP.Trsf)
translateTrsf (V3 x y z) = do
    trsf <- GP.Trsf.new
    vec <- GP.Vec.new x y z
    liftIO $ GP.Trsf.setTranslation trsf vec
    return trsf

mirrorTrsf :: V3 Double -> Acquire (Ptr GP.Trsf)
mirrorTrsf (V3 x y z) = do
    trsf <- GP.Trsf.new
    dir <- GP.Dir.new x y z
    axis <- GP.xoy
    liftIO $ do 
        GP.Ax2.setDirection axis dir
        GP.Trsf.setMirrorAboutAx2 trsf axis
    return trsf
    
instance Transformable Solid where
    scale :: V3 Double -> Solid -> Solid
    scale = fromGTrsfSolid . scaleTrsf

    uScale :: Double -> Solid -> Solid
    uScale = fromTrsfSolid . uScaleTrsf

    rotate :: V3 Double -> Double -> Solid -> Solid
    rotate axis angle = fromTrsfSolid (rotateTrsf axis angle)

    translate :: V3 Double -> Solid -> Solid
    translate = fromTrsfSolid . translateTrsf

    mirror :: V3 Double -> Solid -> Solid
    mirror = fromTrsfSolid . mirrorTrsf

instance Transformable Path where
    scale :: V3 Double -> Path -> Path
    scale = fromGTrsfPath . scaleTrsf

    uScale :: Double -> Path -> Path
    uScale = fromTrsfPath . uScaleTrsf

    rotate :: V3 Double -> Double -> Path -> Path
    rotate axis angle = fromTrsfPath (rotateTrsf axis angle)

    translate :: V3 Double -> Path -> Path
    translate = fromTrsfPath . translateTrsf
    
    mirror :: V3 Double -> Path -> Path
    mirror = fromTrsfPath . mirrorTrsf

        
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

    mirror :: V3 Double -> V3 Double -> V3 Double 
    mirror mirrorVec toMirror = 
        let nm = normalize mirrorVec
        in toMirror - (2 * (nm `dot` toMirror) *^ nm)

