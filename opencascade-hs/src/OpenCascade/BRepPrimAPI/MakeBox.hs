{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeBox
( fromPnts
, solid
, shell
) where

import OpenCascade.BRepPrimAPI.Types (MakeBox)
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeBox)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.GP as GP
import Foreign.Ptr
import Data.Acquire

-- new

foreign import capi unsafe "hs_BRepPrimAPI_MakeBox.h hs_new_BRepPrimAPI_MakeBox_fromPnts" rawFromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt ->  IO (Ptr MakeBox)

fromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr MakeBox)
fromPnts a b = mkAcquire (rawFromPnts a b) deleteMakeBox

-- solid 


foreign import capi unsafe "hs_BRepPrimAPI_MakeBox.h hs_BRepPrimAPI_MakeBox_Solid" rawSolid :: Ptr MakeBox ->  IO (Ptr TopoDS.Solid)

solid :: Ptr MakeBox -> Acquire (Ptr TopoDS.Solid)
solid builder = mkAcquire (rawSolid builder) (deleteShape . castPtr)

-- shell

foreign import capi unsafe "hs_BRepPrimAPI_MakeBox.h hs_BRepPrimAPI_MakeBox_Shell" rawShell :: Ptr MakeBox ->  IO (Ptr TopoDS.Shell)

shell:: Ptr MakeBox -> Acquire (Ptr TopoDS.Shell)
shell builder = mkAcquire (rawShell builder) (deleteShape . castPtr)



