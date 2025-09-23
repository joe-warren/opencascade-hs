module OpenCascade.BRepBuilderAPI.MakeFace 
( MakeFace
, new
, face
, fromFace
, fromSurface
, fromSurfaceAndBounds
, fromSurfaceAndWire
, fromWire
, add
, isDone
, error
) where

import Prelude hiding (error)
import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.Geom.Internal.Context (geomContext)
import OpenCascade.Handle
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import qualified OpenCascade.Geom as Geom
import OpenCascade.BRepBuilderAPI.FaceError (FaceError)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> geomContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeFace.hxx>"
C.include "<TopoDS_Face.hxx>"
C.include "<Geom_Surface.hxx>" 

-- new

new :: Acquire (Ptr MakeFace)
new = mkAcquire createMakeFace deleteMakeFace
  where
    createMakeFace = [C.throwBlock| BRepBuilderAPI_MakeFace* {
      return new BRepBuilderAPI_MakeFace();
    } |]

-- fromFace

fromFace :: Ptr TopoDS.Face -> Acquire (Ptr MakeFace)
fromFace theFace = mkAcquire createMakeFace deleteMakeFace
  where
    createMakeFace = [C.throwBlock| BRepBuilderAPI_MakeFace* {
      return new BRepBuilderAPI_MakeFace(*$(TopoDS_Face* theFace));
    } |]

-- fromSurface

fromSurface :: Ptr (Handle Geom.Surface) -> Double -> Acquire (Ptr MakeFace)
fromSurface surf tolerance = mkAcquire createMakeFace deleteMakeFace
  where
    createMakeFace = 
      let cTolerance = realToFrac tolerance
          voidPtr = castPtr surf
      in [C.throwBlock| BRepBuilderAPI_MakeFace* {
        opencascade::handle<Geom_Surface>* handlePtr = static_cast<opencascade::handle<Geom_Surface>*>($(void* voidPtr));
        return new BRepBuilderAPI_MakeFace(*handlePtr, $(double cTolerance));
      } |]

-- fromSurfaceAndBounds

fromSurfaceAndBounds :: Ptr (Handle Geom.Surface) -> Double -> Double -> Double -> Double -> Double -> Acquire (Ptr MakeFace)
fromSurfaceAndBounds surf uMin uMax vMin vMax tolerance = mkAcquire createMakeFace deleteMakeFace
  where
    createMakeFace = 
      let cUMin = realToFrac uMin
          cUMax = realToFrac uMax
          cVMin = realToFrac vMin
          cVMax = realToFrac vMax
          cTolerance = realToFrac tolerance
          voidPtr = castPtr surf
      in [C.throwBlock| BRepBuilderAPI_MakeFace* {
        opencascade::handle<Geom_Surface>* handlePtr = static_cast<opencascade::handle<Geom_Surface>*>($(void* voidPtr));
        return new BRepBuilderAPI_MakeFace(*handlePtr, $(double cUMin), $(double cUMax), $(double cVMin), $(double cVMax), $(double cTolerance));
      } |]

-- fromSurfaceAndWire

fromSurfaceAndWire :: Ptr (Handle Geom.Surface) -> Ptr TopoDS.Wire -> Bool -> Acquire (Ptr MakeFace)
fromSurfaceAndWire surf wire inside = mkAcquire createMakeFace deleteMakeFace
  where
    createMakeFace = 
      let cInside = boolToCBool inside
          voidPtr = castPtr surf
      in [C.throwBlock| BRepBuilderAPI_MakeFace* {
        opencascade::handle<Geom_Surface>* handlePtr = static_cast<opencascade::handle<Geom_Surface>*>($(void* voidPtr));
        return new BRepBuilderAPI_MakeFace(*handlePtr, *$(TopoDS_Wire* wire), $(bool cInside));
      } |]

-- fromWire

fromWire :: Ptr TopoDS.Wire -> Bool -> Acquire (Ptr MakeFace)
fromWire wire onlyPlane = mkAcquire createMakeFace deleteMakeFace
  where
    createMakeFace = 
      let cOnlyPlane = boolToCBool onlyPlane
      in [C.throwBlock| BRepBuilderAPI_MakeFace* {
        return new BRepBuilderAPI_MakeFace(*$(TopoDS_Wire* wire), $(bool cOnlyPlane));
      } |]

-- add

add :: Ptr MakeFace -> Ptr TopoDS.Wire -> IO ()
add makeFace wireToAdd = [C.throwBlock| void {
  $(BRepBuilderAPI_MakeFace* makeFace)->Add(*$(TopoDS_Wire* wireToAdd));
} |]

-- isDone

isDone :: Ptr MakeFace -> IO Bool
isDone makeFace = do
  result <- [C.throwBlock| bool {
    return $(BRepBuilderAPI_MakeFace* makeFace)->IsDone();
  } |]
  return (cBoolToBool result)

-- error

error :: Ptr MakeFace -> IO FaceError 
error makeFace = do
  result <- [C.throwBlock| int {
    return static_cast<int>($(BRepBuilderAPI_MakeFace* makeFace)->Error());
  } |]
  return (toEnum . fromIntegral $ result)


-- face

face :: Ptr MakeFace -> Acquire (Ptr TopoDS.Face)
face makeFace = mkAcquire createFace (deleteShape . upcast)
  where
    createFace = [C.throwBlock| TopoDS_Face* {
      return new TopoDS_Face($(BRepBuilderAPI_MakeFace* makeFace)->Face());
    } |]