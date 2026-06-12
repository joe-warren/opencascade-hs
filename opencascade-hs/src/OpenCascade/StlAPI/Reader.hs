{-# LANGUAGE CApiFFI #-}
module OpenCascade.StlAPI.Reader
( Reader
, new
, read
) where

import Prelude hiding (read)
import OpenCascade.StlAPI.Types (Reader)
import OpenCascade.StlAPI.Internal.Destructors (deleteReader)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_StlAPI_Reader.h hs_new_StlAPI_Reader" rawNew :: IO (Ptr Reader)

new :: Acquire (Ptr Reader)
new = mkAcquire rawNew deleteReader

foreign import capi unsafe "hs_StlAPI_Reader.h hs_StlAPI_Reader_read" rawRead
    :: Ptr Reader
    -> Ptr TopoDS.Shape
    -> CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CBool)

read :: Ptr Reader -> Ptr TopoDS.Shape -> String -> IO (Bool)
read reader shape filename = cBoolToBool <$> withCString filename (wrapException . rawRead reader shape)