module OpenCascade.Bnd.OBB
( OBB
, new
, center
, xDirection, yDirection, zDirection
, xHSize, yHSize, zHSize
, position
) where

import OpenCascade.Bnd.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.Bnd.Types
import OpenCascade.Bnd.Internal.Destructors (deleteOBB)
import OpenCascade.GP.Types (XYZ, Ax3)
import OpenCascade.GP.Internal.Destructors (deleteXYZ, deleteAx3)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..))
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> gpContext <> bndContext)

C.include "<Bnd_OBB.hxx>"
C.include "<gp_XYZ.hxx>"
C.include "<gp_Ax3.hxx>"

new :: Acquire (Ptr OBB)
new =
  let createOBB = [C.throwBlock| Bnd_OBB* {
        return new Bnd_OBB();
      } |]
  in mkAcquire createOBB deleteOBB

center :: Ptr OBB -> Acquire (Ptr XYZ)
center obb =
  let createCenter = [C.throwBlock| gp_XYZ* {
        return new gp_XYZ($(Bnd_OBB* obb)->Center());
      } |]
  in mkAcquire createCenter deleteXYZ

xDirection :: Ptr OBB -> Acquire (Ptr XYZ)
xDirection obb =
  let createXDirection = [C.throwBlock| gp_XYZ* {
        return new gp_XYZ($(Bnd_OBB* obb)->XDirection());
      } |]
  in mkAcquire createXDirection deleteXYZ

yDirection :: Ptr OBB -> Acquire (Ptr XYZ)
yDirection obb =
  let createYDirection = [C.throwBlock| gp_XYZ* {
        return new gp_XYZ($(Bnd_OBB* obb)->YDirection());
      } |]
  in mkAcquire createYDirection deleteXYZ

zDirection :: Ptr OBB -> Acquire (Ptr XYZ)
zDirection obb =
  let createZDirection = [C.throwBlock| gp_XYZ* {
        return new gp_XYZ($(Bnd_OBB* obb)->ZDirection());
      } |]
  in mkAcquire createZDirection deleteXYZ

xHSize :: Ptr OBB -> IO Double 
xHSize obb = do
  result <- [C.throwBlock| double {
    return $(Bnd_OBB* obb)->XHSize();
  } |]
  return (realToFrac result)

yHSize :: Ptr OBB -> IO Double 
yHSize obb = do
  result <- [C.throwBlock| double {
    return $(Bnd_OBB* obb)->YHSize();
  } |]
  return (realToFrac result)

zHSize :: Ptr OBB -> IO Double 
zHSize obb = do
  result <- [C.throwBlock| double {
    return $(Bnd_OBB* obb)->ZHSize();
  } |]
  return (realToFrac result)

position :: Ptr OBB -> Acquire (Ptr Ax3)
position obb =
  let createPosition = [C.throwBlock| gp_Ax3* {
        return new gp_Ax3($(Bnd_OBB* obb)->Position());
      } |]
  in mkAcquire createPosition deleteAx3