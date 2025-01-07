{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Waterfall.Transforms
( Transformable
, matTransform
, scale
, uScale
, rotate
, translate
, mirror
) where
import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire) 
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Linear (M34, (*^), normalize, dot, (!*), unit, _w, _xyz)
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
import Data.Function ((&))
import Control.Lens ((.~))

-- | Typeclass for objects that can be manipulated in 3D space
class Transformable a where
    -- | Directly transform with a transformation matrix
    matTransform :: M34 Double -> a -> a
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
fromTrsfSolid mkTrsf s = solidFromAcquire $ do 
    solid <- acquireSolid s
    trsf <- mkTrsf 
    BRepBuilderAPI.Transform.transform solid trsf True 

fromGTrsfSolid :: Acquire (Maybe (Ptr GP.GTrsf)) -> Solid -> Solid
fromGTrsfSolid mkTrsf s = solidFromAcquire $ do 
    solid <- acquireSolid s
    trsfMay <- mkTrsf 
    case trsfMay of
        Just trsf -> BRepBuilderAPI.GTransform.gtransform solid trsf True 
        Nothing -> pure solid

fromTrsfPath :: Acquire (Ptr GP.Trsf) -> Path -> Path
fromTrsfPath mkTrsf (Path p) = Path . unsafeFromAcquire $ do 
    path <- toAcquire p
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) =<< BRepBuilderAPI.Transform.transform (upcast path) trsf True 

fromGTrsfPath :: Acquire (Maybe (Ptr GP.GTrsf)) -> Path -> Path
fromGTrsfPath mkTrsf (Path p) = Path . unsafeFromAcquire $ do 
    path <- toAcquire p
    trsfMay <- mkTrsf 
    case trsfMay of
        Just trsf -> (liftIO . unsafeDowncast) =<< BRepBuilderAPI.GTransform.gtransform (upcast path) trsf True 
        Nothing -> pure path

scaleTrsf :: V3 Double -> Acquire (Maybe (Ptr GP.GTrsf))
scaleTrsf v@(V3 x y z ) = 
    if v == V3 1 1 1 
        then pure Nothing
        else do
            trsf <- GP.GTrsf.new 
            liftIO $ do
                GP.GTrsf.setValue trsf 1 1 x
                GP.GTrsf.setValue trsf 2 2 y
                GP.GTrsf.setValue trsf 3 3 z
                GP.GTrsf.setForm trsf
                return . Just $ trsf

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

matrixGTrsf :: M34 Double -> Acquire (Maybe (Ptr GP.GTrsf))
matrixGTrsf (V3 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0)) = pure Nothing
matrixGTrsf (V3 (V4 v11 v12 v13 v14) (V4 v21 v22 v23 v24) (V4 v31 v32 v33 v34)) = do
    trsf <- GP.GTrsf.new
    liftIO $ do  
        GP.GTrsf.setValue trsf 1 1 v11
        GP.GTrsf.setValue trsf 1 2 v12
        GP.GTrsf.setValue trsf 1 3 v13
        GP.GTrsf.setValue trsf 1 4 v14
        GP.GTrsf.setValue trsf 2 1 v21
        GP.GTrsf.setValue trsf 2 2 v22
        GP.GTrsf.setValue trsf 2 3 v23
        GP.GTrsf.setValue trsf 2 4 v24
        GP.GTrsf.setValue trsf 3 1 v31
        GP.GTrsf.setValue trsf 3 2 v32
        GP.GTrsf.setValue trsf 3 3 v33
        GP.GTrsf.setValue trsf 3 4 v34
        GP.GTrsf.setForm trsf
        return . pure $ trsf
    
instance Transformable Solid where
    matTransform :: M34 Double -> Solid -> Solid
    matTransform = fromGTrsfSolid . matrixGTrsf 
    
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
    matTransform :: M34 Double -> Path -> Path
    matTransform = fromGTrsfPath . matrixGTrsf 
    
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
    matTransform :: M34 Double -> V3 Double -> V3 Double
    matTransform m v = m !* (unit _w & _xyz .~ v)

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

