{-# LANGUAGE CApiFFI #-}
module OpenCascade.XSControl.Reader 
( Reader
, readFile
, oneShape
) where

import Prelude hiding (readFile)
import OpenCascade.XSControl.Types (Reader)
import OpenCascade.IFSelect.ReturnStatus (ReturnStatus)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.C.String (CString, withCString)
import Foreign.C (CInt (..))
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_XSControl_Reader.h hs_XSControl_Reader_readFile" rawReadFile :: Ptr Reader -> CString -> IO CInt

readFile :: Ptr Reader -> String -> IO ReturnStatus
readFile reader s = toEnum . fromIntegral <$> withCString s (rawReadFile reader)

foreign import capi unsafe "hs_XSControl_Reader.h hs_XSControl_Reader_oneShape" rawOneShape :: Ptr Reader  -> IO (Ptr TopoDS.Shape) 

oneShape :: Ptr Reader -> Acquire (Ptr TopoDS.Shape)
oneShape reader  = mkAcquire (rawOneShape reader) deleteShape
