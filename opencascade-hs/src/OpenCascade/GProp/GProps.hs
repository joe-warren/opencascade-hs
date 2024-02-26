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
import Foreign.C (CDouble (..))
import OpenCascade.GProp.Types (GProps)
import OpenCascade.GProp.Internal.Destructors (deleteGProps)
import OpenCascade.GP.Types (Pnt, Ax1)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import Data.Acquire 
import Data.Coerce (coerce)

foreign import capi unsafe "hs_GProp_GProps.h hs_new_GProp_GProps" rawNew :: IO (Ptr GProps)

new :: Acquire (Ptr GProps)
new = mkAcquire rawNew deleteGProps

foreign import capi unsafe "hs_GProp_GProps.h hs_new_GProp_GProps" rawFromSystemLocation :: Ptr Pnt -> IO (Ptr GProps)

fromSystemLocation :: Ptr Pnt -> Acquire (Ptr GProps)
fromSystemLocation pnt = mkAcquire (rawFromSystemLocation pnt) deleteGProps

foreign import capi unsafe "hs_GProp_GProps.h hs_GProp_GProps_mass" rawMass :: Ptr GProps -> IO CDouble 

mass :: Ptr GProps -> IO Double
mass = coerce rawMass

foreign import capi unsafe "hs_GProp_GProps.h hs_GProp_GProps_centreOfMass" rawCentreOfMass :: Ptr GProps -> IO (Ptr Pnt)

centreOfMass :: Ptr GProps -> Acquire (Ptr Pnt)
centreOfMass gProps = mkAcquire (rawCentreOfMass gProps) deletePnt

foreign import capi unsafe "hs_GProp_GProps.h hs_GProp_GProps_momentOfInertia" rawMomentOfIntertia :: Ptr GProps -> Ptr Ax1 -> IO CDouble 

momentOfInertia :: Ptr GProps -> Ptr Ax1 -> IO Double
momentOfInertia = coerce rawMomentOfIntertia