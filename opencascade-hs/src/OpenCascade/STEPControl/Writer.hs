{-# LANGUAGE CApiFFI #-}
module OpenCascade.STEPControl.Writer
( new
, setTolerance
, unsetTolerance
, transfer
, write
) where

import OpenCascade.STEPControl.Types (Writer)
import OpenCascade.STEPControl.Internal.Destructors (deleteWriter)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool)
import qualified OpenCascade.IFSelect.ReturnStatus as IFSelect.ReturnStatus
import OpenCascade.STEPControl.StepModelType (StepModelType)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_STEPControl_Writer.h hs_new_STEPControl_Writer" rawNew :: IO (Ptr Writer)

new :: Acquire (Ptr Writer)
new = mkAcquire rawNew deleteWriter

foreign import capi unsafe "hs_STEPControl_Writer.h hs_STEPControl_Writer_setTolerance" rawSetTolerance :: Ptr Writer -> CDouble -> IO ()

setTolerance :: Ptr Writer -> Double -> IO ()
setTolerance = coerce rawSetTolerance 

foreign import capi unsafe "hs_STEPControl_Writer.h hs_STEPControl_Writer_unsetTolerance" unsetTolerance :: Ptr Writer -> IO ()

foreign import capi unsafe "hs_STEPControl_Writer.h hs_STEPControl_Writer_transfer" rawTransfer :: Ptr Writer -> Ptr TopoDS.Shape -> CInt -> CBool -> IO CInt

transfer :: Ptr Writer -> Ptr TopoDS.Shape -> StepModelType -> Bool -> IO IFSelect.ReturnStatus.ReturnStatus
transfer writer shape mode compgraph = toEnum . fromIntegral <$> rawTransfer writer shape (fromIntegral . fromEnum $ mode) (boolToCBool compgraph)

foreign import capi unsafe "hs_STEPControl_Writer.h hs_STEPControl_Writer_write" rawWrite :: Ptr Writer -> CString -> IO CInt

write :: Ptr Writer -> String -> IO IFSelect.ReturnStatus.ReturnStatus
write writer filename = toEnum . fromIntegral <$> withCString filename (rawWrite writer)