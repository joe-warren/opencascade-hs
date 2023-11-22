{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.CompSolid
( CompSolid
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_CompSolid.h hs_new_TopoDS_CompSolid" rawNew :: IO (Ptr CompSolid)

new :: Acquire (Ptr CompSolid)
new = mkAcquire rawNew (deleteShape . castPtr)
