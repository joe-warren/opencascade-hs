{-# LANGUAGE CApiFFI #-}
module OpenCascade.Poly.Internal.Destructors
( deleteHandleTriangulation
, deleteTriangle
) where

import OpenCascade.Poly.Types 
import OpenCascade.Handle (Handle)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_Poly_Triangulation.h hs_delete_Poly_Triangulation" deleteHandleTriangulation :: Ptr (Handle Triangulation) -> IO ()

foreign import capi unsafe "hs_Poly_Triangle.h hs_delete_Poly_Triangle" deleteTriangle :: Ptr Triangle -> IO ()