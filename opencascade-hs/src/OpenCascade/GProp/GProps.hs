{-# LANGUAGE CApiFFI #-}
module OpenCascade.GProp.GProps
( GProps
, new
, fromSystemLocation
, mass
, centreOfMass
, momentOfInertia
) where

import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..), CInt)
import OpenCascade.GProp.Types (GProps)
import OpenCascade.GProp.Internal.Destructors (deleteGProps)
import OpenCascade.GP.Types (Pnt, Ax1)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire
import Data.Coerce (coerce)

foreign import capi unsafe "hs_GProp_GProps.h hs_new_GProp_GProps" rawNew
    :: Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr GProps)

new :: Acquire (Ptr GProps)
new = mkAcquire (wrapException rawNew) deleteGProps

foreign import capi unsafe "hs_GProp_GProps.h hs_new_GProp_GProps_fromSystemLocation" rawFromSystemLocation
    :: Ptr Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr GProps)

fromSystemLocation :: Ptr Pnt -> Acquire (Ptr GProps)
fromSystemLocation pnt = mkAcquire (wrapException $ rawFromSystemLocation pnt) deleteGProps

foreign import capi unsafe "hs_GProp_GProps.h hs_GProp_GProps_mass" rawMass
    :: Ptr GProps
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CDouble

mass :: Ptr GProps -> IO Double
mass props = coerce <$> wrapException (rawMass props)

foreign import capi unsafe "hs_GProp_GProps.h hs_GProp_GProps_centreOfMass" rawCentreOfMass
    :: Ptr GProps
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Pnt)

centreOfMass :: Ptr GProps -> Acquire (Ptr Pnt)
centreOfMass gProps = mkAcquire (wrapException $ rawCentreOfMass gProps) deletePnt

foreign import capi unsafe "hs_GProp_GProps.h hs_GProp_GProps_momentOfInertia" rawMomentOfIntertia
    :: Ptr GProps
    -> Ptr Ax1
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CDouble

momentOfInertia :: Ptr GProps -> Ptr Ax1 -> IO Double
momentOfInertia props ax = coerce <$> wrapException (rawMomentOfIntertia props ax)