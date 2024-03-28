{-# LANGUAGE CApiFFI#-}
module OpenCascade.BRepLib 
( orientClosedSolid
) where 

import OpenCascade.TopoDS.Types (Solid)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (cBoolToBool)

foreign import capi unsafe "hs_BRepLib.h hs_BRepLib_orientClosedSolid" rawOrientClosedSolid ::  Ptr Solid -> IO (CBool)

orientClosedSolid :: Ptr Solid -> IO Bool
orientClosedSolid s = cBoolToBool <$> rawOrientClosedSolid s