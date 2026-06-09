{-# LANGUAGE CApiFFI #-}
module OpenCascade.Poly.Triangulation 
(Triangulation
, new
, nbNodes
, nbTriangles
, node 
, setNode
, triangle
, setTriangle
) where

import OpenCascade.Poly.Types (Triangle, Triangulation)
import OpenCascade.Poly.Internal.Destructors (deleteHandleTriangulation, deleteTriangle)
import OpenCascade.GP.Types as GP
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.Handle (Handle)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_Poly_Triangulation.h hs_new_Poly_Triangulation" rawNew
    :: CInt
    -> CInt
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Triangulation))

new :: Int -> Int -> Bool -> Bool -> Acquire (Ptr (Handle Triangulation))
new nNodes nTriangles hasUVNodes hasNormals = mkAcquire (wrapException $ rawNew (fromIntegral nNodes) (fromIntegral nTriangles) (boolToCBool hasUVNodes) (boolToCBool hasNormals)) deleteHandleTriangulation

foreign import capi unsafe "hs_Poly_Triangulation.h hs_Poly_Triangulation_nbNodes" rawNbNodes :: Ptr (Handle Triangulation) -> IO CInt

nbNodes :: Ptr (Handle Triangulation) -> IO Int
nbNodes tri = fromIntegral <$> rawNbNodes tri

foreign import capi unsafe "hs_Poly_Triangulation.h hs_Poly_Triangulation_nbTriangles" rawNbTriangles :: Ptr (Handle Triangulation) -> IO CInt

nbTriangles :: Ptr (Handle Triangulation) -> IO Int
nbTriangles tri = fromIntegral <$> rawNbTriangles tri

foreign import capi unsafe "hs_Poly_Triangulation.h hs_Poly_Triangulation_node" rawNode
    :: Ptr (Handle Triangulation)
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr GP.Pnt)

node :: Ptr (Handle Triangulation) -> Int -> Acquire (Ptr GP.Pnt)
node tri index = mkAcquire (wrapException $ rawNode tri (fromIntegral index)) deletePnt

foreign import capi unsafe "hs_Poly_Triangulation.h hs_Poly_Triangulation_setNode" rawSetNode
    :: Ptr (Handle Triangulation)
    -> CInt
    -> Ptr GP.Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setNode :: Ptr (Handle Triangulation) -> Int -> Ptr GP.Pnt -> IO ()
setNode tri index pnt = wrapException $ rawSetNode tri (fromIntegral index) pnt

foreign import capi unsafe "hs_Poly_Triangulation.h hs_Poly_Triangulation_triangle" rawTriangle
    :: Ptr (Handle Triangulation)
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Triangle)

triangle :: Ptr (Handle Triangulation) -> Int -> Acquire (Ptr Triangle)
triangle tri index = mkAcquire (wrapException $ rawTriangle tri (fromIntegral index)) deleteTriangle

foreign import capi unsafe "hs_Poly_Triangulation.h hs_Poly_Triangulation_setTriangle" rawSetTriangle
    :: Ptr (Handle Triangulation)
    -> CInt
    -> Ptr Triangle
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setTriangle :: Ptr (Handle Triangulation) -> Int -> Ptr Triangle -> IO ()
setTriangle tri index theTriangle = wrapException $ rawSetTriangle tri (fromIntegral index) theTriangle
