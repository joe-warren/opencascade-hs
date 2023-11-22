{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.Array1
(
    newGPPntArray,
    setValueGPPnt
) where

import Data.Acquire
import Foreign.Ptr
import Foreign.C
import OpenCascade.GP.Types
import OpenCascade.NCollection.Types
import OpenCascade.NCollection.Internal.Destructors

foreign import capi unsafe "hs_NCollection_Array1.h hs_new_NCollection_Array1_gp_Pnt" rawNewGPPntArray :: CInt -> CInt -> IO (Ptr (Array1 Pnt))

newGPPntArray :: Int -> Int -> Acquire (Ptr (Array1 Pnt))
newGPPntArray lo hi = mkAcquire (rawNewGPPntArray (fromIntegral lo) (fromIntegral hi)) deletePntArray

foreign import capi unsafe "hs_NCollection_Array1.h hs_NCollection_Array1_gp_Pnt_setValue" rawSetValueGPPnt :: Ptr (Array1 Pnt) -> CInt -> Ptr Pnt -> IO ()

setValueGPPnt :: Ptr (Array1 Pnt) -> Int -> Ptr Pnt -> IO ()
setValueGPPnt arr i p = rawSetValueGPPnt arr (fromIntegral i) p
