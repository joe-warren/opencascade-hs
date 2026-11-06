{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Lin
( Lin
, new
, fromAx1
, location
, direction
, position
, setLocation
, setDirection
, setPosition
, reverse
, reversed
, angle
, contains
, distancePnt
, distanceLin
, squareDistancePnt
, squareDistanceLin
, normal
, mirror
, mirrored
, mirrorAboutAx1
, mirroredAboutAx1
, mirrorAboutAx2
, mirroredAboutAx2
, rotate
, rotated
, scale
, scaled
, transform
, transformed
, translate
, translated
, translateRelative
, translatedRelative
) where

import Prelude hiding (reverse)
import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire

foreign import capi unsafe "hs_gp_Lin.h hs_new_gp_Lin" rawNew :: Ptr Pnt -> Ptr Dir -> IO (Ptr Lin)

new :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Lin)
new origin dir = mkAcquire (rawNew origin dir) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_new_gp_Lin_fromAx1" rawFromAx1 :: Ptr Ax1 -> IO (Ptr Lin)

fromAx1 :: Ptr Ax1 -> Acquire (Ptr Lin)
fromAx1 axis = mkAcquire (rawFromAx1 axis) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Location" rawLocation :: Ptr Lin -> IO (Ptr Pnt)

location :: Ptr Lin -> Acquire (Ptr Pnt)
location lin = mkAcquire (rawLocation lin) deletePnt

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Direction" rawDirection :: Ptr Lin -> IO (Ptr Dir)

direction :: Ptr Lin -> Acquire (Ptr Dir)
direction lin = mkAcquire (rawDirection lin) deleteDir

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Position" rawPosition :: Ptr Lin -> IO (Ptr Ax1)

position :: Ptr Lin -> Acquire (Ptr Ax1)
position lin = mkAcquire (rawPosition lin) deleteAx1

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_SetLocation" setLocation :: Ptr Lin -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_SetDirection" setDirection :: Ptr Lin -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_SetPosition" setPosition :: Ptr Lin -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Reverse" reverse :: Ptr Lin -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Reversed" rawReversed :: Ptr Lin -> IO (Ptr Lin)

reversed :: Ptr Lin -> Acquire (Ptr Lin)
reversed lin = mkAcquire (rawReversed lin) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Angle" rawAngle :: Ptr Lin -> Ptr Lin -> IO CDouble

angle :: Ptr Lin -> Ptr Lin -> IO Double
angle = coerce rawAngle

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Contains" rawContains :: Ptr Lin -> Ptr Pnt -> CDouble -> IO CBool

contains :: Ptr Lin -> Ptr Pnt -> Double -> IO Bool
contains lin pnt linearTolerance = cBoolToBool <$> rawContains lin pnt (CDouble linearTolerance)

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_DistancePnt" rawDistancePnt :: Ptr Lin -> Ptr Pnt -> IO CDouble

distancePnt :: Ptr Lin -> Ptr Pnt -> IO Double
distancePnt = coerce rawDistancePnt

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_DistanceLin" rawDistanceLin :: Ptr Lin -> Ptr Lin -> IO CDouble

distanceLin :: Ptr Lin -> Ptr Lin -> IO Double
distanceLin = coerce rawDistanceLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_SquareDistancePnt" rawSquareDistancePnt :: Ptr Lin -> Ptr Pnt -> IO CDouble

squareDistancePnt :: Ptr Lin -> Ptr Pnt -> IO Double
squareDistancePnt = coerce rawSquareDistancePnt

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_SquareDistanceLin" rawSquareDistanceLin :: Ptr Lin -> Ptr Lin -> IO CDouble

squareDistanceLin :: Ptr Lin -> Ptr Lin -> IO Double
squareDistanceLin = coerce rawSquareDistanceLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Normal" rawNormal :: Ptr Lin -> Ptr Pnt -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Lin)

normal :: Ptr Lin -> Ptr Pnt -> Acquire (Ptr Lin)
normal lin pnt = mkAcquire (wrapException $ rawNormal lin pnt) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Mirror" mirror :: Ptr Lin -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Mirrored" rawMirrored :: Ptr Lin -> Ptr Pnt -> IO (Ptr Lin)

mirrored :: Ptr Lin -> Ptr Pnt -> Acquire (Ptr Lin)
mirrored lin pnt = mkAcquire (rawMirrored lin pnt) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_MirrorAboutAx1" mirrorAboutAx1 :: Ptr Lin -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_MirroredAboutAx1" rawMirroredAboutAx1 :: Ptr Lin -> Ptr Ax1 -> IO (Ptr Lin)

mirroredAboutAx1 :: Ptr Lin -> Ptr Ax1 -> Acquire (Ptr Lin)
mirroredAboutAx1 lin axis = mkAcquire (rawMirroredAboutAx1 lin axis) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_MirrorAboutAx2" mirrorAboutAx2 :: Ptr Lin -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_MirroredAboutAx2" rawMirroredAboutAx2 :: Ptr Lin -> Ptr Ax2 -> IO (Ptr Lin)

mirroredAboutAx2 :: Ptr Lin -> Ptr Ax2 -> Acquire (Ptr Lin)
mirroredAboutAx2 lin axis = mkAcquire (rawMirroredAboutAx2 lin axis) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Rotate" rawRotate :: Ptr Lin -> Ptr Ax1 -> CDouble -> IO ()

rotate :: Ptr Lin -> Ptr Ax1 -> Double -> IO ()
rotate = coerce rawRotate

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Rotated" rawRotated :: Ptr Lin -> Ptr Ax1 -> CDouble -> IO (Ptr Lin)

rotated :: Ptr Lin -> Ptr Ax1 -> Double -> Acquire (Ptr Lin)
rotated lin axis amount = mkAcquire (rawRotated lin axis (CDouble amount)) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Scale" rawScale :: Ptr Lin -> Ptr Pnt -> CDouble -> IO ()

scale :: Ptr Lin -> Ptr Pnt -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Scaled" rawScaled :: Ptr Lin -> Ptr Pnt -> CDouble -> IO (Ptr Lin)

scaled :: Ptr Lin -> Ptr Pnt -> Double -> Acquire (Ptr Lin)
scaled lin origin amount = mkAcquire (rawScaled lin origin (CDouble amount)) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Transform" transform :: Ptr Lin -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Transformed" rawTransformed :: Ptr Lin -> Ptr Trsf -> IO (Ptr Lin)

transformed :: Ptr Lin -> Ptr Trsf -> Acquire (Ptr Lin)
transformed lin trsf = mkAcquire (rawTransformed lin trsf) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Translate" translate :: Ptr Lin -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Translated" rawTranslated :: Ptr Lin -> Ptr Vec -> IO (Ptr Lin)

translated :: Ptr Lin -> Ptr Vec -> Acquire (Ptr Lin)
translated lin vec = mkAcquire (rawTranslated lin vec) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_TranslateRelative" translateRelative :: Ptr Lin -> Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_TranslatedRelative" rawTranslatedRelative :: Ptr Lin -> Ptr Pnt -> Ptr Pnt -> IO (Ptr Lin)

translatedRelative :: Ptr Lin -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Lin)
translatedRelative lin from to = mkAcquire (rawTranslatedRelative lin from to) deleteLin
