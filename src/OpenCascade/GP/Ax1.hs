{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Ax1
( Ax1
, new
, location
, direction
, setLocation
, setDirection
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new and delete

foreign import capi unsafe "hs_gp_Ax1.h hs_new_gp_Ax1" rawNew :: Ptr Pnt -> Ptr Dir -> IO (Ptr Ax1)

new :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax1)
new origin dir = mkAcquire (rawNew origin dir) deleteAx1

-- getters

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Location" rawLocation :: Ptr Ax1 -> IO (Ptr Pnt)

location :: Ptr Ax1 -> Acquire (Ptr Pnt)
location ax1 = mkAcquire (rawLocation ax1) deletePnt

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Direction" rawDirection :: Ptr Ax1 -> IO (Ptr Dir)

direction :: Ptr Ax1 -> Acquire (Ptr Dir)
direction ax1 = mkAcquire (rawDirection ax1) deleteDir

-- setters

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_SetDirection" setDirection :: Ptr Ax1 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_SetLocation" setLocation :: Ptr Ax1 -> Ptr Pnt -> IO ()
