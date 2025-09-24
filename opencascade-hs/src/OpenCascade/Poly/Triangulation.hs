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

import OpenCascade.Poly.Internal.Context
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.Poly.Types (Triangle, Triangulation)
import OpenCascade.Poly.Internal.Destructors (deleteHandleTriangulation, deleteTriangle)
import OpenCascade.GP.Types as GP
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.Handle (Handle)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)
import Data.Acquire (Acquire, mkAcquire)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> handleContext <> gpContext <> polyContext)

C.include "<Poly_Triangulation.hxx>"
C.include "<Standard_Handle.hxx>"
C.include "<gp_Pnt.hxx>"

new :: Int -> Int -> Bool -> Bool -> Acquire (Ptr (Handle Triangulation))
new nNodes nTriangles hasUVNodes hasNormals =
  let cNNodes = fromIntegral nNodes
      cNTriangles = fromIntegral nTriangles
      cHasUVNodes = boolToCBool hasUVNodes
      cHasNormals = boolToCBool hasNormals
      createTriangulation = [C.throwBlock| opencascade::handle<Poly_Triangulation>* {
        return new opencascade::handle<Poly_Triangulation>(
          new Poly_Triangulation($(int cNNodes), $(int cNTriangles), $(bool cHasUVNodes), $(bool cHasNormals))
        );
      } |]
  in mkAcquire createTriangulation deleteHandleTriangulation

nbNodes :: Ptr (Handle Triangulation) -> IO Int
nbNodes tri = do
  result <- [C.throwBlock| int {
    return (*$(opencascade::handle<Poly_Triangulation>* tri))->NbNodes();
  } |]
  return (fromIntegral result)

nbTriangles :: Ptr (Handle Triangulation) -> IO Int
nbTriangles tri = do
  result <- [C.throwBlock| int {
    return (*$(opencascade::handle<Poly_Triangulation>* tri))->NbTriangles();
  } |]
  return (fromIntegral result)

node :: Ptr (Handle Triangulation) -> Int -> Acquire (Ptr GP.Pnt)
node tri index =
  let cIndex = fromIntegral index
      createNode = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt((*$(opencascade::handle<Poly_Triangulation>* tri))->Node($(int cIndex)));
      } |]
  in mkAcquire createNode deletePnt

setNode :: Ptr (Handle Triangulation) -> Int -> Ptr GP.Pnt -> IO ()
setNode tri index pnt = do
  let cIndex = fromIntegral index
  [C.throwBlock| void {
    (*$(opencascade::handle<Poly_Triangulation>* tri))->SetNode($(int cIndex), *$(gp_Pnt* pnt));
  } |]

triangle :: Ptr (Handle Triangulation) -> Int -> Acquire (Ptr Triangle)
triangle tri index =
  let cIndex = fromIntegral index
      createTriangle = [C.throwBlock| Poly_Triangle* {
        return new Poly_Triangle((*$(opencascade::handle<Poly_Triangulation>* tri))->Triangle($(int cIndex)));
      } |]
  in mkAcquire createTriangle deleteTriangle

setTriangle :: Ptr (Handle Triangulation) -> Int -> Ptr Triangle -> IO ()
setTriangle tri index theTriangle = do
  let cIndex = fromIntegral index
  [C.throwBlock| void {
    (*$(opencascade::handle<Poly_Triangulation>* tri))->SetTriangle($(int cIndex), *$(Poly_Triangle* theTriangle));
  } |] 
