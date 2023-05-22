{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Vertex
( Vertex
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Vertex.h hs_new_TopoDS_Vertex" rawNew :: IO (Ptr Vertex)

new :: Acquire (Ptr Vertex)
new = mkAcquire rawNew (deleteShape . castPtr)
