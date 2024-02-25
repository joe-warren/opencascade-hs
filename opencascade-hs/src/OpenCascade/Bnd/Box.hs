{-# LANGUAGE CApiFFI #-}
module OpenCascade.Bnd.Box
( Box
, new
, cornerMin
, cornerMax
) where

import OpenCascade.Bnd.Types
import OpenCascade.Bnd.Internal.Destructors (deleteBox)
import OpenCascade.GP.Types (Pnt)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
foreign import capi unsafe "hs_Bnd_Box.h hs_new_Bnd_Box" rawNew ::  IO (Ptr Box)

new :: Acquire (Ptr Box)
new = mkAcquire rawNew deleteBox

foreign import capi unsafe "hs_Bnd_Box.h hs_Bnd_Box_cornerMin" rawCornerMin :: Ptr Box -> IO (Ptr Pnt)

cornerMin :: Ptr Box -> Acquire (Ptr Pnt)
cornerMin box = mkAcquire (rawCornerMin box) deletePnt

foreign import capi unsafe "hs_Bnd_Box.h hs_Bnd_Box_cornerMax" rawCornerMax :: Ptr Box -> IO (Ptr Pnt)

cornerMax :: Ptr Box -> Acquire (Ptr Pnt)
cornerMax box = mkAcquire (rawCornerMax box) deletePnt