{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWGltf.CafWriter
( CafWriter
, new
, perform
) where

import OpenCascade.RWGltf.Types (CafWriter)
import OpenCascade.RWGltf.Internal.Destructors (deleteCafWriter)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Handle (Handle)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.TColStd.Types as TColStd
import qualified OpenCascade.Message.Types as Message

foreign import capi unsafe "hs_RWGltf_CafWriter.h hs_new_RWGltf_CafWriter" rawNew :: CString -> CBool -> IO (Ptr CafWriter)

new :: String -> Bool -> Acquire (Ptr CafWriter)
new filepath binary = mkAcquire (withCString filepath $ \str -> rawNew str (boolToCBool binary)) deleteCafWriter

foreign import capi unsafe "hs_RWGltf_CafWriter.h hs_RWGltf_CafWriter_Perform" perform :: Ptr CafWriter -> Ptr (Handle TDocStd.Document) -> Ptr (TColStd.IndexedDataMapOfStringString) -> Ptr (Message.ProgressRange) -> IO ()