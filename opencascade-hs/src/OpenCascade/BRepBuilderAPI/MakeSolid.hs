{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeSolid
( MakeSolid
, new
, add
, solid
) where
import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Inheritance (upcast)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire

-- new

foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_new_BRepBuilderAPI_MakeSolid" rawNew :: IO (Ptr MakeSolid)

new :: Acquire (Ptr MakeSolid)
new = mkAcquire rawNew deleteMakeSolid

-- add
foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_BRepBuilderAPI_MakeSolid_add" rawAdd
    :: Ptr MakeSolid
    -> Ptr TopoDS.Shell
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

add :: Ptr MakeSolid -> Ptr TopoDS.Shell -> IO ()
add builder shell = wrapException $ rawAdd builder shell

foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_BRepBuilderAPI_MakeSolid_solid" rawSolid
    :: Ptr MakeSolid
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Solid)

solid :: Ptr MakeSolid -> Acquire (Ptr TopoDS.Solid)
solid builder = mkAcquire (wrapException $ rawSolid builder) (deleteShape . upcast)

