module OpenCascade.GP.Internal.Destructors
( deletePnt
, deletePnt2d
, deleteAx1
, deleteAx2
, deleteAx2d
, deleteAx3
, deleteDir
, deleteDir2d
, deleteVec
, deleteVec2d
, deleteTrsf
, deleteTrsf2d
, deleteGTrsf
, deleteXYZ
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> gpContext)

C.include "<gp_Pnt.hxx>"
C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Ax2d.hxx>"
C.include "<gp_Ax3.hxx>"
C.include "<gp_Dir.hxx>"
C.include "<gp_Dir2d.hxx>"
C.include "<gp_Vec.hxx>"
C.include "<gp_Vec2d.hxx>"
C.include "<gp_Trsf.hxx>"
C.include "<gp_Trsf2d.hxx>"
C.include "<gp_GTrsf.hxx>"
C.include "<gp_XYZ.hxx>"

deletePnt :: Ptr Pnt -> IO ()
deletePnt ptr = [C.throwBlock| void {
  delete $(gp_Pnt* ptr);
} |]
deletePnt2d :: Ptr Pnt2d -> IO ()
deletePnt2d ptr = [C.throwBlock| void {
  delete $(gp_Pnt2d* ptr);
} |]

deleteAx1 :: Ptr Ax1 -> IO ()
deleteAx1 ptr = [C.throwBlock| void {
  delete $(gp_Ax1* ptr);
} |]

deleteAx2 :: Ptr Ax2 -> IO ()
deleteAx2 ptr = [C.throwBlock| void {
  delete $(gp_Ax2* ptr);
} |]

deleteAx2d :: Ptr Ax2d -> IO ()
deleteAx2d ptr = [C.throwBlock| void {
  delete $(gp_Ax2d* ptr);
} |]

deleteAx3 :: Ptr Ax3 -> IO ()
deleteAx3 ptr = [C.throwBlock| void {
  delete $(gp_Ax3* ptr);
} |]

deleteDir :: Ptr Dir -> IO ()
deleteDir ptr = [C.throwBlock| void {
  delete $(gp_Dir* ptr);
} |]

deleteDir2d :: Ptr Dir2d -> IO ()
deleteDir2d ptr = [C.throwBlock| void {
  delete $(gp_Dir2d* ptr);
} |]

deleteVec :: Ptr Vec -> IO ()
deleteVec ptr = [C.throwBlock| void {
  delete $(gp_Vec* ptr);
} |]

deleteVec2d :: Ptr Vec2d -> IO ()
deleteVec2d ptr = [C.throwBlock| void {
  delete $(gp_Vec2d* ptr);
} |]

deleteTrsf :: Ptr Trsf -> IO ()
deleteTrsf ptr = [C.throwBlock| void {
  delete $(gp_Trsf* ptr);
} |]

deleteGTrsf :: Ptr GTrsf -> IO ()
deleteGTrsf ptr = [C.throwBlock| void {
  delete $(gp_GTrsf* ptr);
} |]

deleteTrsf2d :: Ptr Trsf2d -> IO ()
deleteTrsf2d ptr = [C.throwBlock| void {
  delete $(gp_Trsf2d* ptr);
} |]

deleteXYZ :: Ptr XYZ -> IO ()
deleteXYZ ptr = [C.throwBlock| void {
  delete $(gp_XYZ* ptr);
} |]

