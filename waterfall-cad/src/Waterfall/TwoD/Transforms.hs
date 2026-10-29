{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Waterfall.TwoD.Transforms
( -- * Transformations
 Transformable2D
, matTransform2D
, rotate2D
, scale2D
, uScale2D
, translate2D
, mirror2D
-- * Optics
, _translated2D
, _scaled2D
, _uScaled2D
, _rotated2D
, _mirrored2D
) where

import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.Internal.Finalizers (acquireForeignPtr, unsafeFromAcquirePtr)
import Waterfall.Internal.NearZero (nearZero)
import Linear (M23, M22, (*^), normalize, dot, V3 (..), V2 (..), (!*), _xy, _z, _y, _x, unit, transpose, (!*!), (!-!), (*!!), identity)
import qualified OpenCascade.GP.Trsf as GP.Trsf
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.GTrsf as GP.GTrsf
import qualified OpenCascade.GP.Ax1 as GP.Ax1
import qualified OpenCascade.GP.Ax2 as GP.Ax2
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.BRepBuilderAPI.MakeShape  as BRepBuilderAPI.MakeShape
import qualified OpenCascade.BRepBuilderAPI.Transform  as BRepBuilderAPI.Transform
import qualified OpenCascade.BRepBuilderAPI.GTransform  as BRepBuilderAPI.GTransform
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire
import Foreign.Ptr
import Waterfall.TwoD.Internal.Shape (Shape(..))
import Data.Function ((&))
import Control.Lens ((.~), (%~), Iso', iso, (^.), over)
import Waterfall.Internal.Path.Common (RawPath(..))
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Control.Monad (when)

-- | Typeclass for objects that can be manipulated in 2D space
class Transformable2D a where
    -- | Directly transform with a transformation matrix
    matTransform2D :: M23 Double -> a -> a
    -- | Rotate by an angle (in radians) about the origin
    rotate2D :: Double -> a -> a
    -- | Scale by different amounts along the x and y axes
    scale2D :: V2 Double -> a -> a
    -- | Scale uniformly along both axes
    uScale2D :: Double -> a -> a
    -- | Translate by a distance in 2D space
    translate2D :: V2 Double -> a -> a
    -- | Mirror in the line, which passes through the origin, perpendicular to the specified vector
    -- 
    -- Note that in order to maintain consistency with 'Waterfall.Transforms.Transformable',
    -- the mirror is in the line / perpendicular / to the vector, not in the line / parallel / to the vector
    mirror2D :: V2 Double -> a -> a

fromTrsfPath :: (V2 Double -> V2 Double) -> Acquire (Ptr GP.Trsf) -> Path2D -> Path2D
fromTrsfPath _ mkTrsf (Path2D (ComplexRawPath p)) = Path2D . ComplexRawPath . unsafeFromAcquirePtr $ do 
    path <- acquireForeignPtr p
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) 
        =<< BRepBuilderAPI.MakeShape.shape
        =<< fmap upcast
            (BRepBuilderAPI.Transform.fromShapeTrsfAndCopy (upcast path) trsf True)
fromTrsfPath f _ (Path2D (SinglePointRawPath v)) = Path2D . SinglePointRawPath $ (v & _xy %~ f)
fromTrsfPath _ _ (Path2D EmptyRawPath) = Path2D EmptyRawPath

fromTrsfShape :: Acquire (Ptr GP.Trsf) -> Shape -> Shape
fromTrsfShape mkTrsf (Shape theRawShape) = Shape . unsafeFromAcquirePtr $ do 
    shape <- acquireForeignPtr theRawShape
    trsf <- mkTrsf 
    builder <- BRepBuilderAPI.Transform.fromShapeTrsfAndCopy shape trsf True 
    isNegative <- liftIO $ GP.Trsf.isNegative trsf
    res <- BRepBuilderAPI.MakeShape.shape (upcast builder)
    if isNegative then TopoDS.Shape.reversed res else pure res
    
fromGTrsfPath :: (V2 Double -> V2 Double) -> Acquire (Ptr GP.GTrsf) -> Path2D -> Path2D
fromGTrsfPath _ mkTrsf (Path2D (ComplexRawPath p)) = Path2D . ComplexRawPath . unsafeFromAcquirePtr  $ do 
    path <- acquireForeignPtr p
    trsf <- mkTrsf 
    (liftIO . unsafeDowncast) 
        =<< BRepBuilderAPI.MakeShape.shape
        =<< fmap upcast
            (BRepBuilderAPI.GTransform.fromShapeGTrsfAndCopy (upcast path) trsf True)

fromGTrsfPath f _ (Path2D (SinglePointRawPath v)) = Path2D . SinglePointRawPath $ (v & _xy %~ f)
fromGTrsfPath _ _ (Path2D EmptyRawPath) = Path2D EmptyRawPath

fromGTrsfShape :: Acquire (Ptr GP.GTrsf) -> Shape -> Shape
fromGTrsfShape mkTrsf (Shape theRawShape) = Shape . unsafeFromAcquirePtr $ do 
    shape <- acquireForeignPtr theRawShape 
    trsf <- mkTrsf 
    isNegative <- liftIO $ GP.GTrsf.isNegative trsf
    res <- BRepBuilderAPI.MakeShape.shape 
        =<< fmap upcast
        (BRepBuilderAPI.GTransform.fromShapeGTrsfAndCopy shape trsf True)
    if isNegative then TopoDS.Shape.reversed res else pure res

isOrthogonalAndScale :: M22 Double -> Bool
isOrthogonalAndScale m =
    let p = m !*! transpose m
        s = p ^. _x . _x
        in not (nearZero s) && all (all (nearZero . (/s))) (p !-! (s *!! identity))

matrixGTrsf :: M23 Double -> Maybe (Either (Acquire (Ptr GP.Trsf)) (Acquire (Ptr GP.GTrsf)))
matrixGTrsf (V2 (V3 1 0 0) (V3 0 1 0)) = Nothing
matrixGTrsf (V2 (V3 x 0 0) (V3 0 y 0)) | nearZero (x-y)= Just . Left . uScaleTrsf $ x
matrixGTrsf m@(V2 (V3 v11 v12 v13) (V3 v21 v22 v23)) 
    | isOrthogonalAndScale (fmap (^. _xy ) m) = Just . Left $ do
        trsf <- GP.Trsf.new
        let s = sqrt (abs (v11 * v22 - v12 * v21))
        liftIO $ GP.Trsf.setValues trsf v11 v12 0 v13 v21 v22 0 v23 0 0 s 0
        return trsf
    | otherwise = Just . Right $ do
        trsf <- GP.GTrsf.new
        liftIO $ do  
            GP.GTrsf.setValue trsf 1 1 v11
            GP.GTrsf.setValue trsf 1 2 v12
            GP.GTrsf.setValue trsf 1 4 v13
            GP.GTrsf.setValue trsf 2 1 v21
            GP.GTrsf.setValue trsf 2 2 v22
            GP.GTrsf.setValue trsf 2 4 v23
            return trsf

rotateTrsf :: Double -> Acquire (Ptr GP.Trsf)
rotateTrsf angle = do
    trsf <- GP.Trsf.new
    o <- GP.origin
    dir <- GP.Dir.new 0 0 1
    axis <- GP.Ax1.new o dir
    liftIO $ GP.Trsf.setRotationAboutAxisAngle trsf axis angle
    return trsf

applyScaleTrsfs :: 
    (Acquire (Ptr GP.Trsf) -> a -> a) 
    -> (Acquire (Ptr GP.GTrsf) -> a -> a)
    -> (Maybe (Acquire (Ptr GP.Trsf)), Maybe (Acquire (Ptr GP.GTrsf)))
    -> a -> a
applyScaleTrsfs applyTrsf applyGTrsf (maybeTrsf, maybeGTrsf) = 
     maybe id applyTrsf maybeTrsf . maybe id applyGTrsf maybeGTrsf

scaleTrsfs :: V2 Double -> (Maybe (Acquire (Ptr GP.Trsf)), Maybe (Acquire (Ptr GP.GTrsf)))
scaleTrsfs (V2 1 1) = (Nothing, Nothing)
scaleTrsfs v@(V2 x y) = 
    let isUniform = nearZero (abs x - abs y)
        bothArePositive = x >= 0 && y >= 0
    in if isUniform && bothArePositive 
        then (Just $ uScaleTrsf x, Nothing)
        else ( if bothArePositive
                then Nothing 
                else Just $ do
                    trsf <- if isUniform 
                        then uScaleTrsf (abs x)
                        else GP.Trsf.new
                    when (x < 0) $ do
                        mirrorX <- mirrorTrsf (unit _x)
                        liftIO $ GP.Trsf.multiply trsf mirrorX
                    when (y < 0) $ do
                        mirrorY <- mirrorTrsf (unit _y)
                        liftIO $ GP.Trsf.multiply trsf mirrorY
                    return trsf

            , if isUniform 
                then Nothing
                else Just $ do
                    trsf <- GP.GTrsf.new 
                    liftIO $ do
                        GP.GTrsf.setValue trsf 1 1 x
                        GP.GTrsf.setValue trsf 2 2 y
                        GP.GTrsf.setValue trsf 3 3 1
                        return trsf


        ) 
            

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
    
mirrorTrsf :: V2 Double -> Acquire (Ptr GP.Trsf)
mirrorTrsf (V2 x y) = do 
    trsf <- GP.Trsf.new
    dir <- GP.Dir.new x y 0
    axis <- GP.xoy
    liftIO $ do
        GP.Ax2.setDirection axis dir
        GP.Trsf.setMirrorAboutAx2 trsf axis
    return trsf

transformPathSinglePointPaths :: (V3 Double -> V3 Double) -> Path2D -> Path2D
transformPathSinglePointPaths f (Path2D (SinglePointRawPath v)) = Path2D . SinglePointRawPath . f $ v 
transformPathSinglePointPaths _ p = p 

instance Transformable2D Path2D where
    matTransform2D :: M23 Double -> Path2D -> Path2D
    matTransform2D m = 
        let transformPnt = matTransform2D m
         in maybe id (either (fromTrsfPath transformPnt) (fromGTrsfPath transformPnt)) $ matrixGTrsf m

    rotate2D :: Double -> Path2D -> Path2D
    rotate2D a = fromTrsfPath (rotate2D a) (rotateTrsf a)
    
    scale2D :: V2 Double -> Path2D -> Path2D
    scale2D s = 
        let transformPnt = over _xy (scale2D s)
         in transformPathSinglePointPaths transformPnt 
            . applyScaleTrsfs (fromTrsfPath id) (fromGTrsfPath id) (scaleTrsfs s)

    uScale2D :: Double -> Path2D -> Path2D
    uScale2D s = fromTrsfPath (uScale2D s) (uScaleTrsf s)

    translate2D :: V2 Double -> Path2D -> Path2D
    translate2D v = fromTrsfPath (translate2D v) (translateTrsf v)

    mirror2D :: V2 Double -> Path2D -> Path2D
    mirror2D v = fromTrsfPath (mirror2D v) (mirrorTrsf v)

instance Transformable2D Shape where
    matTransform2D :: M23 Double -> Shape -> Shape
    matTransform2D = maybe id (either fromTrsfShape fromGTrsfShape) . matrixGTrsf

    rotate2D :: Double -> Shape -> Shape
    rotate2D = fromTrsfShape . rotateTrsf  
    
    scale2D :: V2 Double -> Shape -> Shape
    scale2D = applyScaleTrsfs fromTrsfShape fromGTrsfShape . scaleTrsfs

    uScale2D :: Double -> Shape -> Shape
    uScale2D = fromTrsfShape . uScaleTrsf

    translate2D :: V2 Double -> Shape -> Shape
    translate2D = fromTrsfShape .translateTrsf

    mirror2D :: V2 Double -> Shape -> Shape
    mirror2D = fromTrsfShape . mirrorTrsf

instance Transformable2D (V2 Double) where
    matTransform2D :: M23 Double -> V2 Double -> V2 Double
    matTransform2D m v = m !* (unit _z & _xy .~ v)

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

    mirror2D :: V2 Double -> V2 Double -> V2 Double 
    mirror2D mirrorVec toMirror = 
        let nm = normalize mirrorVec
        in toMirror - (2 * (nm `dot` toMirror) *^ nm)

-- | Every translation is an isomorphism
_translated2D :: Transformable2D t => V2 Double -> Iso' t t
_translated2D v = iso (translate2D v) (translate2D (negate v))

-- | A scale by @v@ as an isomorphism
--
-- Returns 'Nothing' when any component of @v@ is (near) zero,
-- as a scale that collapses an axis has no inverse.
_scaled2D :: Transformable2D t => V2 Double -> Maybe (Iso' t t)
_scaled2D v = if any nearZero v
    then Nothing
    else Just $ iso (scale2D v) (scale2D (1/v))

-- | A scale by @s@ as an isomorphism
--
-- Returns 'Nothing' when @s@ is (near) zero,
--  as a scale that collapses everything to the origin has no inverse.
_uScaled2D :: Transformable2D t => Double -> Maybe (Iso' t t)
_uScaled2D s = if nearZero s
    then Nothing
    else Just $ iso (uScale2D s) (uScale2D (1/s))

-- | Every rotation is an isomorphism
_rotated2D :: Transformable2D t => Double -> Iso' t t
_rotated2D angle = iso (rotate2D angle) (rotate2D (negate angle))

-- | Every mirror is an isomorphism
_mirrored2D :: Transformable2D t => V2 Double -> Iso' t t
_mirrored2D v = let f = mirror2D v in iso f f