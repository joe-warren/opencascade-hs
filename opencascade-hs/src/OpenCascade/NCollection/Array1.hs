module OpenCascade.NCollection.Array1
(
    newGPPntArray,
    setValueGPPnt
) where

import OpenCascade.NCollection.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import Data.Acquire
import Foreign.Ptr
import Foreign.C
import OpenCascade.GP.Types
import OpenCascade.NCollection.Types
import OpenCascade.NCollection.Internal.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> gpContext <> nCollectionContext)

C.include "<NCollection_Array1.hxx>"
C.include "<gp_Pnt.hxx>"

newGPPntArray :: Int -> Int -> Acquire (Ptr (Array1 Pnt))
newGPPntArray lo hi =
  let cLo = fromIntegral lo
      cHi = fromIntegral hi
      createArray = [C.throwBlock| NCollection_Array1<gp_Pnt>* {
        return new NCollection_Array1<gp_Pnt>($(int cLo), $(int cHi));
      } |]
  in mkAcquire createArray deletePntArray

setValueGPPnt :: Ptr (Array1 Pnt) -> Int -> Ptr Pnt -> IO ()
setValueGPPnt arr i p = do
  let cIndex = fromIntegral i
  [C.throwBlock| void {
    $(NCollection_Array1<gp_Pnt>* arr)->SetValue($(int cIndex), *$(gp_Pnt* p));
  } |]
