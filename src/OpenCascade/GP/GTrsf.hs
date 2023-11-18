{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.GTrsf
( GTrsf
, new
, setValue
, setForm
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 

foreign import capi unsafe "hs_gp_GTrsf.h hs_new_gp_GTrsf" rawNew ::IO (Ptr GTrsf)

new :: Acquire (Ptr GTrsf)
new = mkAcquire rawNew deleteGTrsf

foreign import capi unsafe "hs_gp_GTrsf.h hs_gp_GTrsf_setValue" rawSetValue :: Ptr GTrsf -> CInt -> CInt -> CDouble -> IO () 

setValue :: Ptr GTrsf -> Int -> Int -> Double -> IO ()
setValue trsf row column value = rawSetValue trsf (fromIntegral row) (fromIntegral column) (coerce value)

foreign import capi unsafe "hs_gp_GTrsf.h hs_gp_GTrsf_setForm" setForm :: Ptr GTrsf -> IO () 
