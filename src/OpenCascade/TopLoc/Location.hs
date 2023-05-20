{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopLoc.Location
( Location
, new
, fromGPTrsf
, isIdentity
, firstPower
, nextLocation
, inverted
, multiplied
, divided
, predivided
, powered
, isEqual
, isDifferent
, clear
, toGPTrsf
) where

import OpenCascade.TopLoc.Types
import OpenCascade.TopLoc.Internal.Destructors
import OpenCascade.GP.Internal.Destructors (deleteTrsf)
import Foreign.C
import Foreign.Ptr
import Data.Acquire 
import qualified OpenCascade.GP as GP

-- new 

foreign import capi unsafe "hs_TopLoc_Location.h hs_new_TopLoc_Location" rawNew :: IO (Ptr Location)

new :: Acquire (Ptr Location)
new = mkAcquire rawNew deleteLocation

-- from GP.Trsf

foreign import capi unsafe "hs_TopLoc_Location.h hs_new_TopLoc_Location_fromGPTrsf" rawFromGPTrsf :: Ptr GP.Trsf -> IO (Ptr Location)

fromGPTrsf :: Ptr (GP.Trsf) -> Acquire (Ptr Location)
fromGPTrsf t = mkAcquire (rawFromGPTrsf t) deleteLocation

-- isIdentity

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_IsIdentity" rawIsIdentity :: Ptr Location -> IO (CBool)

isIdentity :: Ptr Location -> IO Bool
isIdentity l = (/=0) <$>  rawIsIdentity l

-- firstPower 
--
foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_FirstPower" rawFirstPower :: Ptr Location -> IO (CInt)

firstPower :: Ptr Location -> IO Int
firstPower l = fromIntegral <$> rawFirstPower l 

-- nextLocation

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_NextLocation" rawNextLocation :: Ptr Location -> IO (Ptr Location)

nextLocation :: Ptr Location -> Acquire (Ptr Location)
nextLocation l = mkAcquire (rawNextLocation l) deleteLocation

-- inverted

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_Inverted" rawInverted :: Ptr Location -> IO (Ptr Location)

inverted :: Ptr Location -> Acquire (Ptr Location)
inverted l = mkAcquire (rawInverted l) deleteLocation

-- multiplied

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_Multiplied" rawMultiplied :: Ptr Location -> Ptr Location -> IO (Ptr Location)

multiplied :: Ptr Location -> Ptr Location -> Acquire (Ptr Location)
multiplied a b = mkAcquire (rawMultiplied a b) deleteLocation


-- divided

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_Divided" rawDivided :: Ptr Location -> Ptr Location -> IO (Ptr Location)

divided :: Ptr Location -> Ptr Location -> Acquire (Ptr Location)
divided a b = mkAcquire (rawDivided a b) deleteLocation


-- predivided

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_Predivided" rawPredivided :: Ptr Location -> Ptr Location -> IO (Ptr Location)

predivided :: Ptr Location -> Ptr Location -> Acquire (Ptr Location)
predivided a b = mkAcquire (rawPredivided a b) deleteLocation

-- powered

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_Powered" rawPowered :: Ptr Location -> CInt -> IO (Ptr Location)

powered :: Ptr Location -> Int -> Acquire (Ptr Location)
powered l p = mkAcquire (rawPowered l (fromIntegral p)) deleteLocation

-- isEqual

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_IsEqual" rawIsEqual :: Ptr Location -> Ptr Location -> IO (CBool)

isEqual :: Ptr Location -> Ptr Location -> IO Bool
isEqual a b = (/=0) <$> rawIsEqual a b


-- isDifferent

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_IsDifferent" rawIsDifferent :: Ptr Location -> Ptr Location -> IO (CBool)

isDifferent :: Ptr Location -> Ptr Location -> IO Bool
isDifferent a b = (/=0) <$> rawIsDifferent a b


-- clear

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_Clear" clear :: Ptr Location -> IO ()

-- toGPTrsf
--

foreign import capi unsafe "hs_TopLoc_Location.h hs_TopLoc_Location_toGPTrsf" rawToGPTrsf :: Ptr Location -> IO (Ptr GP.Trsf)

toGPTrsf :: Ptr Location -> Acquire (Ptr GP.Trsf)
toGPTrsf l = mkAcquire (rawToGPTrsf l) deleteTrsf
