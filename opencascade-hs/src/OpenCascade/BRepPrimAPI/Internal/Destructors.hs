module OpenCascade.BRepPrimAPI.Internal.Destructors 
( deleteMakeBox
, deleteMakeRevol
, deleteMakeSphere
, deleteMakeCylinder
, deleteMakeCone
, deleteMakePrism
) where

import OpenCascade.BRepPrimAPI.Types
import OpenCascade.BRepPrimAPI.Internal.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakeBox.hxx>"
C.include "<BRepPrimAPI_MakeRevol.hxx>"
C.include "<BRepPrimAPI_MakeSphere.hxx>"
C.include "<BRepPrimAPI_MakeCylinder.hxx>"
C.include "<BRepPrimAPI_MakeCone.hxx>"
C.include "<BRepPrimAPI_MakePrism.hxx>"

deleteMakeBox :: Ptr MakeBox -> IO ()
deleteMakeBox makeBox = [C.throwBlock| void {
  delete $(BRepPrimAPI_MakeBox* makeBox);
} |]

deleteMakeRevol :: Ptr MakeRevol -> IO ()
deleteMakeRevol makeRevol = [C.throwBlock| void {
  delete $(BRepPrimAPI_MakeRevol* makeRevol);
} |]

deleteMakeSphere :: Ptr MakeSphere -> IO ()
deleteMakeSphere makeSphere = [C.throwBlock| void {
  delete $(BRepPrimAPI_MakeSphere* makeSphere);
} |]

deleteMakeCylinder :: Ptr MakeCylinder -> IO ()
deleteMakeCylinder makeCylinder = [C.throwBlock| void {
  delete $(BRepPrimAPI_MakeCylinder* makeCylinder);
} |]

deleteMakeCone :: Ptr MakeCone -> IO ()
deleteMakeCone makeCone = [C.throwBlock| void {
  delete $(BRepPrimAPI_MakeCone* makeCone);
} |]

deleteMakePrism :: Ptr MakePrism -> IO ()
deleteMakePrism makePrism = [C.throwBlock| void {
  delete $(BRepPrimAPI_MakePrism* makePrism);
} |]


