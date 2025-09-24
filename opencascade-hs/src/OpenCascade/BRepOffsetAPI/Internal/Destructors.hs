module OpenCascade.BRepOffsetAPI.Internal.Destructors
( deleteMakePipe
, deleteMakeOffsetShape
, deleteThruSections
) where

import OpenCascade.BRepOffsetAPI.Internal.Context
import OpenCascade.BRepOffsetAPI.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> brepOffsetAPIContext)

C.include "<BRepOffsetAPI_MakePipe.hxx>"
C.include "<BRepOffsetAPI_MakeOffsetShape.hxx>"
C.include "<BRepOffsetAPI_ThruSections.hxx>"

deleteMakePipe :: Ptr MakePipe -> IO ()
deleteMakePipe pipePtr = [C.throwBlock| void {
  delete $(BRepOffsetAPI_MakePipe* pipePtr);
} |]

deleteMakeOffsetShape :: Ptr MakeOffsetShape -> IO ()
deleteMakeOffsetShape offsetPtr = [C.throwBlock| void {
  delete $(BRepOffsetAPI_MakeOffsetShape* offsetPtr);
} |]

deleteThruSections :: Ptr ThruSections -> IO ()
deleteThruSections sectionsPtr = [C.throwBlock| void {
  delete $(BRepOffsetAPI_ThruSections* sectionsPtr);
} |]
