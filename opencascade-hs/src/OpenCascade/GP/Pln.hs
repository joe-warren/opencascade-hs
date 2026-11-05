{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Pln 
( Pln
, new
, fromAx3
, fromPntAndDir
, fromCartesianEquation
, setAxis
, setLocation
, setPosition
, vReverse
, uReverse
, direct
, coefficients
, axis
, location
, position
, distancePnt
, distanceLin
, distancePln
, squareDistancePnt
, squareDistanceLin
, squareDistancePln
, xAxis
, yAxis
, containsPnt
, containsLin
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

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_gp_Pln.h hs_new_gp_Pln" rawNew ::  IO (Ptr Pln)

new :: Acquire (Ptr Pln)
new = mkAcquire rawNew deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_new_gp_Pln_fromAx3" rawNewFromAx3 :: Ptr Ax3 -> IO (Ptr Pln)

fromAx3 :: Ptr Ax3 -> Acquire (Ptr Pln)
fromAx3 ax = mkAcquire (rawNewFromAx3 ax) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_new_gp_Pln_fromPntAndDir" rawNewFromPntAndDir :: Ptr Pnt -> Ptr Dir -> IO (Ptr Pln)

fromPntAndDir :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Pln)
fromPntAndDir pnt dir = mkAcquire (rawNewFromPntAndDir pnt dir) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_new_gp_Pln_fromCartesianEquation" rawNewFromCartesianEquation :: CDouble -> CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Pln)

fromCartesianEquation :: Double -> Double -> Double -> Double -> Acquire (Ptr Pln)
fromCartesianEquation a b c d = mkAcquire (wrapException $ rawNewFromCartesianEquation (CDouble a) (CDouble b) (CDouble c) (CDouble d)) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_SetAxis" rawSetAxis :: Ptr Pln -> Ptr Ax1 -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setAxis :: Ptr Pln -> Ptr Ax1 -> IO ()
setAxis pln ax = wrapException $ rawSetAxis pln ax

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_SetLocation" setLocation :: Ptr Pln -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_SetPosition" setPosition :: Ptr Pln -> Ptr Ax3 -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_VReverse" vReverse :: Ptr Pln -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_UReverse" uReverse :: Ptr Pln -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Direct" rawDirect :: Ptr Pln -> IO CBool

direct :: Ptr Pln -> IO Bool
direct pln = cBoolToBool <$> rawDirect pln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Coefficients" rawCoefficients :: Ptr Pln -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

coefficients :: Ptr Pln -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()
coefficients = coerce rawCoefficients

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Axis" rawAxis :: Ptr Pln -> IO (Ptr Ax1)

axis :: Ptr Pln -> Acquire (Ptr Ax1)
axis pln = mkAcquire (rawAxis pln) deleteAx1

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Location" rawLocation :: Ptr Pln -> IO (Ptr Pnt)

location :: Ptr Pln -> Acquire (Ptr Pnt)
location pln = mkAcquire (rawLocation pln) deletePnt

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Position" rawPosition :: Ptr Pln -> IO (Ptr Ax3)

position :: Ptr Pln -> Acquire (Ptr Ax3)
position pln = mkAcquire (rawPosition pln) deleteAx3

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_DistancePnt" rawDistancePnt :: Ptr Pln -> Ptr Pnt -> IO CDouble

distancePnt :: Ptr Pln -> Ptr Pnt -> IO Double
distancePnt = coerce rawDistancePnt

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_DistanceLin" rawDistanceLin :: Ptr Pln -> Ptr Lin -> IO CDouble

distanceLin :: Ptr Pln -> Ptr Lin -> IO Double
distanceLin = coerce rawDistanceLin

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_DistancePln" rawDistancePln :: Ptr Pln -> Ptr Pln -> IO CDouble

distancePln :: Ptr Pln -> Ptr Pln -> IO Double
distancePln = coerce rawDistancePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_SquareDistancePnt" rawSquareDistancePnt :: Ptr Pln -> Ptr Pnt -> IO CDouble

squareDistancePnt :: Ptr Pln -> Ptr Pnt -> IO Double
squareDistancePnt = coerce rawSquareDistancePnt

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_SquareDistanceLin" rawSquareDistanceLin :: Ptr Pln -> Ptr Lin -> IO CDouble

squareDistanceLin :: Ptr Pln -> Ptr Lin -> IO Double
squareDistanceLin = coerce rawSquareDistanceLin

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_SquareDistancePln" rawSquareDistancePln :: Ptr Pln -> Ptr Pln -> IO CDouble

squareDistancePln :: Ptr Pln -> Ptr Pln -> IO Double
squareDistancePln = coerce rawSquareDistancePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_XAxis" rawXAxis :: Ptr Pln -> IO (Ptr Ax1)

xAxis :: Ptr Pln -> Acquire (Ptr Ax1)
xAxis pln = mkAcquire (rawXAxis pln) deleteAx1

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_YAxis" rawYAxis :: Ptr Pln -> IO (Ptr Ax1)

yAxis :: Ptr Pln -> Acquire (Ptr Ax1)
yAxis pln = mkAcquire (rawYAxis pln) deleteAx1


foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_ContainsPnt" rawContainsPnt :: Ptr Pln -> Ptr Pnt -> CDouble -> IO CBool

containsPnt :: Ptr Pln -> Ptr Pnt -> Double -> IO Bool
containsPnt pln pnt tolerance = cBoolToBool <$> rawContainsPnt pln pnt (coerce tolerance)

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_ContainsLin" rawContainsLin :: Ptr Pln -> Ptr Lin -> CDouble -> CDouble -> IO CBool

containsLin :: Ptr Pln -> Ptr Lin -> Double -> Double -> IO Bool
containsLin pln lin linearTolerance angularTolerance = cBoolToBool <$> coerce rawContainsLin pln lin linearTolerance angularTolerance

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Mirror" mirror :: Ptr Pln -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Mirrored" rawMirrored :: Ptr Pln -> Ptr Pnt -> IO (Ptr Pln)

mirrored :: Ptr Pln -> Ptr Pnt -> Acquire (Ptr Pln)
mirrored pln pnt = mkAcquire (rawMirrored pln pnt) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_MirrorAboutAx1" mirrorAboutAx1 :: Ptr Pln -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_MirroredAboutAx1" rawMirroredAboutAx1 :: Ptr Pln -> Ptr Ax1 -> IO (Ptr Pln)

mirroredAboutAx1 :: Ptr Pln -> Ptr Ax1 -> Acquire (Ptr Pln)
mirroredAboutAx1 pln ax = mkAcquire (rawMirroredAboutAx1 pln ax) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_MirrorAboutAx2" mirrorAboutAx2 :: Ptr Pln -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_MirroredAboutAx2" rawMirroredAboutAx2 :: Ptr Pln -> Ptr Ax2 -> IO (Ptr Pln)

mirroredAboutAx2 :: Ptr Pln -> Ptr Ax2 -> Acquire (Ptr Pln)
mirroredAboutAx2 pln ax = mkAcquire (rawMirroredAboutAx2 pln ax) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Rotate" rawRotate :: Ptr Pln -> Ptr Ax1 -> CDouble -> IO ()

rotate :: Ptr Pln -> Ptr Ax1 -> Double -> IO ()
rotate = coerce rawRotate

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Rotated" rawRotated :: Ptr Pln -> Ptr Ax1 -> CDouble -> IO (Ptr Pln)

rotated :: Ptr Pln -> Ptr Ax1 -> Double -> Acquire (Ptr Pln)
rotated pln ax amount = mkAcquire (rawRotated pln ax (CDouble amount)) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Scale" rawScale :: Ptr Pln -> Ptr Pnt -> CDouble -> IO ()

scale :: Ptr Pln -> Ptr Pnt -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Scaled" rawScaled :: Ptr Pln -> Ptr Pnt -> CDouble -> IO (Ptr Pln)

scaled :: Ptr Pln -> Ptr Pnt -> Double -> Acquire (Ptr Pln)
scaled pln origin amount = mkAcquire (rawScaled pln origin (CDouble amount)) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Transform" transform :: Ptr Pln -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Transformed" rawTransformed :: Ptr Pln -> Ptr Trsf -> IO (Ptr Pln)

transformed :: Ptr Pln -> Ptr Trsf -> Acquire (Ptr Pln)
transformed pln trsf = mkAcquire (rawTransformed pln trsf) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Translate" translate :: Ptr Pln -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_Translated" rawTranslated :: Ptr Pln -> Ptr Vec -> IO (Ptr Pln)

translated :: Ptr Pln -> Ptr Vec -> Acquire (Ptr Pln)
translated pln vec = mkAcquire (rawTranslated pln vec) deletePln

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_TranslateRelative" translateRelative :: Ptr Pln -> Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Pln.h hs_gp_Pln_TranslatedRelative" rawTranslatedRelative :: Ptr Pln -> Ptr Pnt -> Ptr Pnt -> IO (Ptr Pln)

translatedRelative :: Ptr Pln -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Pln)
translatedRelative pln from to = mkAcquire (rawTranslatedRelative pln from to) deletePln
