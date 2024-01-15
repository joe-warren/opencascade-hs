{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.Internal.Destructors
( deletePntArray
) where

import OpenCascade.NCollection.Types
import OpenCascade.GP.Types
import Foreign.Ptr

foreign import capi unsafe "hs_NCollection_Array1.h hs_delete_NCollection_Array1_gp_Pnt" deletePntArray :: Ptr (Array1 Pnt) -> IO ()

