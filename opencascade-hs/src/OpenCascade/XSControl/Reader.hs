{-# LANGUAGE CApiFFI #-}
module OpenCascade.XSControl.Reader 
( Reader
, readFile
, transferRoots
, oneShape
) where

import Prelude hiding (readFile)
import OpenCascade.XSControl.Types (Reader)
import OpenCascade.IFSelect.ReturnStatus (ReturnStatus)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.C.String (CString, withCString)
import Foreign.C (CInt (..), CBool (..))
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_XSControl_Reader.h hs_XSControl_Reader_readFile" rawReadFile 
    :: Ptr Reader
    -> CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CInt

readFile :: Ptr Reader -> String -> IO ReturnStatus
readFile reader s = toEnum . fromIntegral <$> withCString s (wrapException . rawReadFile reader)

foreign import capi unsafe "hs_XSControl_Reader.h hs_XSControl_Reader_transferRoots" rawTransferRoots 
    :: Ptr Reader 
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CBool)

transferRoots :: Ptr Reader -> IO Bool
transferRoots reader = cBoolToBool <$> wrapException (rawTransferRoots reader)

foreign import capi unsafe "hs_XSControl_Reader.h hs_XSControl_Reader_oneShape" rawOneShape
    :: Ptr Reader  
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape) 

oneShape :: Ptr Reader -> Acquire (Ptr TopoDS.Shape)
oneShape reader = mkAcquire (wrapException $ rawOneShape reader) deleteShape
