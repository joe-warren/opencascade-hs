{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepMesh.IncrementalMesh
( fromShapeAndLinDeflection
, perform
) where 

import OpenCascade.BRepMesh.Types (IncrementalMesh)
import OpenCascade.BRepMesh.Internal.Destructors (deleteIncrementalMesh)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Data.Acquire
import Data.Coerce (coerce)


foreign import capi unsafe "hs_BRepMesh_IncrementalMesh.h hs_BRepMesh_IncrementalMesh_fromShapeAndLinDeflection" rawFromShapeAndLinDeflection :: Ptr TopoDS.Shape -> Double ->  IO (Ptr IncrementalMesh)

fromShapeAndLinDeflection :: Ptr TopoDS.Shape -> Double -> Acquire (Ptr IncrementalMesh)
fromShapeAndLinDeflection shape linDeflection = mkAcquire (rawFromShapeAndLinDeflection shape (coerce linDeflection)) deleteIncrementalMesh

foreign import capi unsafe "hs_BRepMesh_IncrementalMesh.h hs_BRepMesh_IncrementalMesh_Perform" perform :: Ptr IncrementalMesh -> IO ()
