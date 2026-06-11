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
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_NCollection_Array1.h hs_new_NCollection_Array1_gp_Pnt" rawNewGPPntArray
    :: CInt
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Array1 Pnt))

newGPPntArray :: Int -> Int -> Acquire (Ptr (Array1 Pnt))
newGPPntArray lo hi = mkAcquire (wrapException $ rawNewGPPntArray (fromIntegral lo) (fromIntegral hi)) deletePntArray

foreign import capi unsafe "hs_NCollection_Array1.h hs_NCollection_Array1_gp_Pnt_setValue" rawSetValueGPPnt
    :: Ptr (Array1 Pnt)
    -> CInt
    -> Ptr Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setValueGPPnt :: Ptr (Array1 Pnt) -> Int -> Ptr Pnt -> IO ()
setValueGPPnt arr i p = wrapException $ rawSetValueGPPnt arr (fromIntegral i) p
