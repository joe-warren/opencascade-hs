module OpenCascade.Bnd.Box
( Box
, new
, cornerMin
, cornerMax
) where

import OpenCascade.Bnd.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.Bnd.Types
import OpenCascade.Bnd.Internal.Destructors (deleteBox)
import OpenCascade.GP.Types (Pnt)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> gpContext <> bndContext)

C.include "<Bnd_Box.hxx>"
C.include "<gp_Pnt.hxx>"

new :: Acquire (Ptr Box)
new =
  let createBox = [C.throwBlock| Bnd_Box* {
        return new Bnd_Box();
      } |]
  in mkAcquire createBox deleteBox

cornerMin :: Ptr Box -> Acquire (Ptr Pnt)
cornerMin box =
  let createCornerMin = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt($(Bnd_Box* box)->CornerMin());
      } |]
  in mkAcquire createCornerMin deletePnt

cornerMax :: Ptr Box -> Acquire (Ptr Pnt)
cornerMax box =
  let createCornerMax = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt($(Bnd_Box* box)->CornerMax());
      } |]
  in mkAcquire createCornerMax deletePnt