{-# LANGUAGE CApiFFI #-}
module OpenCascade.STEPCAFControl.Writer
( Writer
, new
, setColorMode
, setNameMode
, transfer
, write
) where

import OpenCascade.STEPCAFControl.Types (Writer)
import OpenCascade.STEPCAFControl.Internal.Destructors (deleteWriter)
import qualified OpenCascade.TDocStd.Types as TDocStd
import OpenCascade.Handle (Handle)
import OpenCascade.STEPControl.StepModelType (StepModelType)
import qualified OpenCascade.IFSelect.ReturnStatus as IFSelect.ReturnStatus
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_STEPCAFControl_Writer.h hs_new_STEPCAFControl_Writer" rawNew :: IO (Ptr Writer)

new :: Acquire (Ptr Writer)
new = mkAcquire rawNew deleteWriter

foreign import capi unsafe "hs_STEPCAFControl_Writer.h hs_STEPCAFControl_Writer_setColorMode" rawSetColorMode :: Ptr Writer -> CBool -> IO ()

setColorMode :: Ptr Writer -> Bool -> IO ()
setColorMode writer = rawSetColorMode writer . boolToCBool

foreign import capi unsafe "hs_STEPCAFControl_Writer.h hs_STEPCAFControl_Writer_setNameMode" rawSetNameMode :: Ptr Writer -> CBool -> IO ()

setNameMode :: Ptr Writer -> Bool -> IO ()
setNameMode writer = rawSetNameMode writer . boolToCBool

foreign import capi unsafe "hs_STEPCAFControl_Writer.h hs_STEPCAFControl_Writer_transfer" rawTransfer
    :: Ptr Writer
    -> Ptr (Handle TDocStd.Document)
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

transfer :: Ptr Writer -> Ptr (Handle TDocStd.Document) -> StepModelType -> IO Bool
transfer writer doc mode = cBoolToBool <$> wrapException (rawTransfer writer doc (fromIntegral . fromEnum $ mode))

foreign import capi unsafe "hs_STEPCAFControl_Writer.h hs_STEPCAFControl_Writer_write" rawWrite
    :: Ptr Writer
    -> CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CInt

write :: Ptr Writer -> String -> IO IFSelect.ReturnStatus.ReturnStatus
write writer filename = toEnum . fromIntegral <$> withCString filename (wrapException . rawWrite writer)
