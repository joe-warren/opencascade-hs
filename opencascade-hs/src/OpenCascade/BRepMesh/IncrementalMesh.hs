{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepMesh.IncrementalMesh
( fromShapeAndLinDeflection
, perform
) where 

import OpenCascade.BRepMesh.Types (IncrementalMesh)
import OpenCascade.BRepMesh.Internal.Destructors (deleteIncrementalMesh)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire
import Data.Coerce (coerce)


foreign import capi unsafe "hs_BRepMesh_IncrementalMesh.h hs_BRepMesh_IncrementalMesh_fromShapeAndLinDeflection" rawFromShapeAndLinDeflection
    :: Ptr TopoDS.Shape
    -> Double
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr IncrementalMesh)

fromShapeAndLinDeflection :: Ptr TopoDS.Shape -> Double -> Acquire (Ptr IncrementalMesh)
fromShapeAndLinDeflection shape linDeflection = mkAcquire (wrapException $ rawFromShapeAndLinDeflection shape (coerce linDeflection)) deleteIncrementalMesh

foreign import capi unsafe "hs_BRepMesh_IncrementalMesh.h hs_BRepMesh_IncrementalMesh_Perform" rawPerform
    :: Ptr IncrementalMesh
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

perform :: Ptr IncrementalMesh -> IO ()
perform mesh = wrapException $ rawPerform mesh
