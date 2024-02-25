{-# LANGUAGE CApiFFI#-}
module OpenCascade.BRepBndLib 
( add
) where 

import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.Bnd.Types (Box)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_add" rawAdd ::  Ptr Shape -> Ptr Box -> CBool -> IO ()

add :: Ptr Shape  -> Ptr Box -> Bool -> IO ()
add shape box useTriangulation = rawAdd shape box (boolToCBool useTriangulation)