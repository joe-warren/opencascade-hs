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
import Foreign.C (CInt, CBool (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Handle (Handle)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.NCollection.Types as NCollection
import qualified OpenCascade.TCollection.Types as TCollection
import qualified OpenCascade.Message.Types as Message
import OpenCascade.Internal.Bool (cBoolToBool)

foreign import capi unsafe "hs_RWObj_CafWriter.h hs_new_RWObj_CafWriter" rawNew :: CString -> IO (Ptr CafWriter)

new :: String -> Acquire (Ptr CafWriter)
new filepath = mkAcquire (withCString filepath rawNew) deleteCafWriter

foreign import capi unsafe "hs_RWObj_CafWriter.h hs_RWObj_CafWriter_Perform" rawPerform
    :: Ptr CafWriter
    -> Ptr (Handle TDocStd.Document)
    -> Ptr (NCollection.IndexedDataMap TCollection.AsciiString TCollection.AsciiString)
    -> Ptr (Message.ProgressRange)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

perform 
    :: Ptr CafWriter 
    -> Ptr (Handle TDocStd.Document)
    -> Ptr (NCollection.IndexedDataMap TCollection.AsciiString TCollection.AsciiString)
    -> Ptr (Message.ProgressRange)
    -> IO Bool
perform writer doc fileInfo progress = fmap cBoolToBool . wrapException $ rawPerform writer doc fileInfo progress