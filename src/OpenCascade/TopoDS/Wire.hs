{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Wire
( Wire
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Wire.h hs_new_TopoDS_Wire" rawNew :: IO (Ptr Wire)

new :: Acquire (Ptr Wire)
new = mkAcquire rawNew (deleteShape . castPtr)
