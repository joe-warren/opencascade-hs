{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Solid
( Solid
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Solid.h hs_new_TopoDS_Solid" rawNew :: IO (Ptr Solid)

new :: Acquire (Ptr Solid)
new = mkAcquire rawNew (deleteShape . castPtr)
