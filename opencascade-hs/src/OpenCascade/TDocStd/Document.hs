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
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)

foreign import capi unsafe "hs_TDocStd_Document.h hs_new_TDocStd_Document" rawNew
    :: CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Document))

fromStorageFormat :: String -> Acquire (Ptr (Handle Document))
fromStorageFormat fmt = mkAcquire (withCString fmt $ \s -> wrapException $ rawNew s) deleteDocumentHandle

foreign import capi unsafe "hs_TDocStd_Document.h hs_TDocStd_Document_main" rawMain
    :: Ptr (Handle Document)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Label)

main :: Ptr (Handle Document) -> Acquire (Ptr Label)
main doc = mkAcquire (wrapException $ rawMain doc) deleteLabel