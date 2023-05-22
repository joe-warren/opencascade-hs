{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Compound
( Compound
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Compound.h hs_new_TopoDS_Compound" rawNew :: IO (Ptr Compound)

new :: Acquire (Ptr Compound)
new = mkAcquire rawNew (deleteShape . castPtr)
