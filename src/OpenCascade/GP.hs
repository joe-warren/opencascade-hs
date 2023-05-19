{-# LANGUAGE CApiFFI #-}
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
import OpenCascade.GP.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- origin

foreign import capi unsafe "hs_gp.h hs_gp_Origin" rawOrigin :: IO (Ptr Pnt)

origin :: Acquire (Ptr Pnt)
origin = mkAcquire (rawOrigin) deletePnt

-- cardinal directions

foreign import capi unsafe "hs_gp.h hs_gp_DX" rawDX :: IO (Ptr Dir)

dx :: Acquire (Ptr Dir)
dx = mkAcquire (rawDX) deleteDir

foreign import capi unsafe "hs_gp.h hs_gp_DY" rawDY :: IO (Ptr Dir)

dy :: Acquire (Ptr Dir)
dy = mkAcquire (rawDY) deleteDir


foreign import capi unsafe "hs_gp.h hs_gp_DZ" rawDZ :: IO (Ptr Dir)

dz :: Acquire (Ptr Dir)
dz = mkAcquire (rawDZ) deleteDir

-- cardinal axes

foreign import capi unsafe "hs_gp.h hs_gp_OX" rawOX :: IO (Ptr Ax1)

ox :: Acquire (Ptr Ax1)
ox = mkAcquire (rawOX) deleteAx1

foreign import capi unsafe "hs_gp.h hs_gp_OY" rawOY :: IO (Ptr Ax1)

oy :: Acquire (Ptr Ax1)
oy = mkAcquire (rawOY) deleteAx1


foreign import capi unsafe "hs_gp.h hs_gp_OZ" rawOZ :: IO (Ptr Ax1)

oz :: Acquire (Ptr Ax1)
oz = mkAcquire (rawOZ) deleteAx1

-- axes - 2

foreign import capi unsafe "hs_gp.h hs_gp_XOY" rawXOY :: IO (Ptr Ax2)

xoy :: Acquire (Ptr Ax2)
xoy = mkAcquire (rawXOY) deleteAx2

foreign import capi unsafe "hs_gp.h hs_gp_YOZ" rawYOZ :: IO (Ptr Ax2)

yoz :: Acquire (Ptr Ax2)
yoz = mkAcquire (rawYOZ) deleteAx2


foreign import capi unsafe "hs_gp.h hs_gp_ZOX" rawZOX :: IO (Ptr Ax2)

zox :: Acquire (Ptr Ax2)
zox = mkAcquire (rawZOX) deleteAx2

-- origin 2d

foreign import capi unsafe "hs_gp.h hs_gp_Origin2d" rawOrigin2d :: IO (Ptr Pnt2d)

origin2d :: Acquire (Ptr Pnt2d)
origin2d = mkAcquire (rawOrigin2d) deletePnt2d

-- cardinal directions (2d)

foreign import capi unsafe "hs_gp.h hs_gp_DX2d" rawDX2d :: IO (Ptr Dir2d)

dx2d :: Acquire (Ptr Dir2d)
dx2d = mkAcquire (rawDX2d) deleteDir2d

foreign import capi unsafe "hs_gp.h hs_gp_DY2d" rawDY2d :: IO (Ptr Dir2d)

dy2d :: Acquire (Ptr Dir2d)
dy2d = mkAcquire (rawDY2d) deleteDir2d

-- cardinal axes (2d)

foreign import capi unsafe "hs_gp.h hs_gp_OX2d" rawOX2d :: IO (Ptr Ax2d)

ox2d :: Acquire (Ptr Ax2d)
ox2d = mkAcquire (rawOX2d) deleteAx2d

foreign import capi unsafe "hs_gp.h hs_gp_OY2d" rawOY2d :: IO (Ptr Ax2d)

oy2d :: Acquire (Ptr Ax2d)
oy2d = mkAcquire (rawOY2d) deleteAx2d
