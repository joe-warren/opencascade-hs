{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Lin
( Lin
, new
, location
, direction
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_gp_Lin.h hs_new_gp_Lin" rawNew :: Ptr Pnt -> Ptr Dir -> IO (Ptr Lin)

new :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Lin)
new origin dir = mkAcquire (rawNew origin dir) deleteLin

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Location" rawLocation :: Ptr Lin -> IO (Ptr Pnt)

location :: Ptr Lin -> Acquire (Ptr Pnt)
location lin = mkAcquire (rawLocation lin) deletePnt

foreign import capi unsafe "hs_gp_Lin.h hs_gp_Lin_Direction" rawDirection :: Ptr Lin -> IO (Ptr Dir)

direction :: Ptr Lin -> Acquire (Ptr Dir)
direction lin = mkAcquire (rawDirection lin) deleteDir
