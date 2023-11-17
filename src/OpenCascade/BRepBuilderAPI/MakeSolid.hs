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
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_new_BRepBuilderAPI_MakeSolid" rawNew :: IO (Ptr MakeSolid)

new :: Acquire (Ptr MakeSolid)
new = mkAcquire rawNew deleteMakeSolid

-- add 
foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_BRepBuilderAPI_MakeSolid_add" add :: Ptr MakeSolid -> Ptr TopoDS.Shell -> IO ()

foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_BRepBuilderAPI_MakeSolid_solid" rawSolid :: Ptr MakeSolid -> IO (Ptr TopoDS.Solid)

solid :: Ptr MakeSolid -> Acquire (Ptr TopoDS.Solid)
solid builder = mkAcquire (rawSolid builder) (deleteShape . upcast)  

