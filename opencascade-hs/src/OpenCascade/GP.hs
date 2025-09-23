module OpenCascade.GP 
( origin
, dx
, dy
, dz
, ox
, oy
, oz
, xoy
, yoz
, zox
, origin2d
, dx2d
, dy2d
, ox2d
, oy2d
, module OpenCascade.GP.Types
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<gp_Dir.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Dir2d.hxx>"
C.include "<gp_Ax2d.hxx>" 

-- origin

origin :: Acquire (Ptr Pnt)
origin = mkAcquire createOrigin deletePnt
  where
    createOrigin = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt(gp::Origin());
    } |]

-- cardinal directions

dx :: Acquire (Ptr Dir)
dx = mkAcquire createDX deleteDir
  where
    createDX = [C.throwBlock| gp_Dir* {
      return new gp_Dir(gp::DX());
    } |]

dy :: Acquire (Ptr Dir)
dy = mkAcquire createDY deleteDir
  where
    createDY = [C.throwBlock| gp_Dir* {
      return new gp_Dir(gp::DY());
    } |]


dz :: Acquire (Ptr Dir)
dz = mkAcquire createDZ deleteDir
  where
    createDZ = [C.throwBlock| gp_Dir* {
      return new gp_Dir(gp::DZ());
    } |]

-- cardinal axes

ox :: Acquire (Ptr Ax1)
ox = mkAcquire createOX deleteAx1
  where
    createOX = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1(gp::OX());
    } |]

oy :: Acquire (Ptr Ax1)
oy = mkAcquire createOY deleteAx1
  where
    createOY = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1(gp::OY());
    } |]


oz :: Acquire (Ptr Ax1)
oz = mkAcquire createOZ deleteAx1
  where
    createOZ = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1(gp::OZ());
    } |]

-- axes - 2

xoy :: Acquire (Ptr Ax2)
xoy = mkAcquire createXOY deleteAx2
  where
    createXOY = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2(gp::XOY());
    } |]

yoz :: Acquire (Ptr Ax2)
yoz = mkAcquire createYOZ deleteAx2
  where
    createYOZ = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2(gp::YOZ());
    } |]


zox :: Acquire (Ptr Ax2)
zox = mkAcquire createZOX deleteAx2
  where
    createZOX = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2(gp::ZOX());
    } |]

-- origin 2d

origin2d :: Acquire (Ptr Pnt2d)
origin2d = mkAcquire createOrigin2d deletePnt2d
  where
    createOrigin2d = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d(gp::Origin2d());
    } |]

-- cardinal directions (2d)

dx2d :: Acquire (Ptr Dir2d)
dx2d = mkAcquire createDX2d deleteDir2d
  where
    createDX2d = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d(gp::DX2d());
    } |]

dy2d :: Acquire (Ptr Dir2d)
dy2d = mkAcquire createDY2d deleteDir2d
  where
    createDY2d = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d(gp::DY2d());
    } |]

-- cardinal axes (2d)

ox2d :: Acquire (Ptr Ax2d)
ox2d = mkAcquire createOX2d deleteAx2d
  where
    createOX2d = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d(gp::OX2d());
    } |]

oy2d :: Acquire (Ptr Ax2d)
oy2d = mkAcquire createOY2d deleteAx2d
  where
    createOY2d = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d(gp::OY2d());
    } |]
