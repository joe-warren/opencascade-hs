module OpenCascade.Poly.Triangle 
( Triangle
, fromIndices
, value
, setValue
) where

import OpenCascade.Poly.Internal.Context
import OpenCascade.Poly.Types (Triangle)
import OpenCascade.Poly.Internal.Destructors (deleteTriangle)
import Foreign.C (CInt (..))
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> polyContext)

C.include "<Poly_Triangle.hxx>"

fromIndices :: Int -> Int -> Int -> Acquire (Ptr Triangle)
fromIndices n1 n2 n3 =
  let cN1 = fromIntegral n1
      cN2 = fromIntegral n2
      cN3 = fromIntegral n3
      createTriangle = [C.throwBlock| Poly_Triangle* {
        return new Poly_Triangle($(int cN1), $(int cN2), $(int cN3));
      } |]
  in mkAcquire createTriangle deleteTriangle

value :: Ptr Triangle -> Int -> IO Int
value tri index = do
  let cIndex = fromIntegral index
  result <- [C.throwBlock| int {
    return $(Poly_Triangle* tri)->Value($(int cIndex));
  } |]
  return (fromIntegral result)

setValue :: Ptr Triangle -> Int -> Int -> IO ()
setValue tri index node = do
  let cIndex = fromIntegral index
      cNode = fromIntegral node
  [C.throwBlock| void {
    $(Poly_Triangle* tri)->Set($(int cIndex), $(int cNode));
  } |]
