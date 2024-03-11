{-# LANGUAGE CApiFFI #-}
module OpenCascade.TDocStd.Internal.Destructors (deleteDocumentHandle) where

import OpenCascade.Handle (Handle)
import OpenCascade.TDocStd.Types (Document)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_TDocStd_Document.h hs_delete_TDocStd_Document" deleteDocumentHandle :: Ptr (Handle Document) -> IO ()