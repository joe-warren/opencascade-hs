{-# LANGUAGE CApiFFI #-}
module OpenCascade.Poly.Triangle 
( Triangle
, fromIndices
, value
, setValue
) where

import OpenCascade.Poly.Types (Triangle)
import OpenCascade.Poly.Internal.Destructors (deleteTriangle)
import Foreign.C (CInt (..))
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_Poly_Triangle.h hs_new_Poly_Triangle_fromIndices" rawFromIndices :: CInt -> CInt -> CInt -> IO (Ptr Triangle)

fromIndices :: Int -> Int -> Int -> Acquire (Ptr Triangle)
fromIndices n1 n2 n3 = mkAcquire (rawFromIndices (fromIntegral n1) (fromIntegral n2) (fromIntegral n3)) deleteTriangle

foreign import capi unsafe "hs_Poly_Triangle.h hs_Poly_Triangle_value" rawValue :: Ptr Triangle -> CInt -> IO CInt

value :: Ptr Triangle -> Int -> IO Int
value tri index = fromIntegral <$> rawValue tri (fromIntegral index)

foreign import capi unsafe "hs_Poly_Triangle.h hs_Poly_Triangle_setValue" rawSetValue :: Ptr Triangle -> CInt -> CInt -> IO ()

setValue :: Ptr Triangle -> Int -> Int -> IO ()
setValue tri index node = rawSetValue tri (fromIntegral index) (fromIntegral node)
