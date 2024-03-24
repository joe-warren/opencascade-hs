{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWObj.CafWriter
( CafWriter
, new
, perform
) where

import OpenCascade.RWObj.Types (CafWriter)
import OpenCascade.RWObj.Internal.Destructors (deleteCafWriter)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Handle (Handle)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.TColStd.Types as TColStd
import qualified OpenCascade.Message.Types as Message

foreign import capi unsafe "hs_RWObj_CafWriter.h hs_new_RWObj_CafWriter" rawNew :: CString -> IO (Ptr CafWriter)

new :: String -> Acquire (Ptr CafWriter)
new filepath = mkAcquire (withCString filepath rawNew) deleteCafWriter

foreign import capi unsafe "hs_RWObj_CafWriter.h hs_RWObj_CafWriter_Perform" perform :: Ptr CafWriter -> Ptr (Handle TDocStd.Document) -> Ptr (TColStd.IndexedDataMapOfStringString) -> Ptr (Message.ProgressRange) -> IO ()