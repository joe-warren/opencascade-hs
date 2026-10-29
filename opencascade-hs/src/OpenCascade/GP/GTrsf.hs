{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.GTrsf
( GTrsf
, new
, setValue
, setForm
, isNegative
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire

foreign import capi unsafe "hs_gp_GTrsf.h hs_new_gp_GTrsf" rawNew ::IO (Ptr GTrsf)

new :: Acquire (Ptr GTrsf)
new = mkAcquire rawNew deleteGTrsf

foreign import capi unsafe "hs_gp_GTrsf.h hs_gp_GTrsf_setValue" rawSetValue :: Ptr GTrsf -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setValue :: Ptr GTrsf -> Int -> Int -> Double -> IO ()
setValue trsf row column value = wrapException $ rawSetValue trsf (fromIntegral row) (fromIntegral column) (coerce value)

foreign import capi unsafe "hs_gp_GTrsf.h hs_gp_GTrsf_setForm" rawSetForm :: Ptr GTrsf -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setForm :: Ptr GTrsf -> IO ()
setForm trsf = wrapException $ rawSetForm trsf


foreign import capi unsafe "hs_gp_GTrsf.h hs_gp_GTrsf_isNegative" rawIsNegative :: Ptr GTrsf -> Ptr CInt -> Ptr (Ptr ()) -> IO (CBool)

isNegative :: Ptr GTrsf -> IO Bool
isNegative gtrsf = fmap cBoolToBool . wrapException $ rawIsNegative gtrsf
