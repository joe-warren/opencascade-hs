{-# LANGUAGE CApiFFI #-}
module OpenCascade.TDocStd.Document 
( Document
, fromStorageFormat
, main 
) where

import OpenCascade.TDocStd.Types (Document)
import OpenCascade.TDocStd.Internal.Destructors (deleteDocumentHandle)
import OpenCascade.TDF.Types (Label)
import OpenCascade.TDF.Internal.Destructors (deleteLabel)
import OpenCascade.Handle (Handle)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)

foreign import capi unsafe "hs_TDocStd_Document.h hs_new_TDocStd_Document" rawNew :: CString -> IO (Ptr (Handle Document))

fromStorageFormat :: String -> Acquire (Ptr (Handle Document))
fromStorageFormat fmt = mkAcquire (withCString fmt rawNew) deleteDocumentHandle

foreign import capi unsafe "hs_TDocStd_Document.h hs_TDocStd_Document_main" rawMain :: Ptr (Handle Document) -> IO (Ptr Label)

main :: Ptr (Handle Document) -> Acquire (Ptr Label)
main doc = mkAcquire (rawMain doc) deleteLabel  