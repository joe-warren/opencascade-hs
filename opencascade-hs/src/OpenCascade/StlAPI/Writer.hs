{-# LANGUAGE CApiFFI #-}
module OpenCascade.StlAPI.Writer
( new
, setAsciiMode
, write
) where

import OpenCascade.StlAPI.Types (Writer)
import OpenCascade.StlAPI.Internal.Destructors (deleteWriter)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)

foreign import capi unsafe "hs_StlAPI_Writer.h hs_new_StlAPI_Writer" rawNew :: IO (Ptr Writer)

new :: Acquire (Ptr Writer)
new = mkAcquire rawNew deleteWriter

foreign import capi unsafe "hs_StlAPI_Writer.h hs_StlAPI_Writer_setAsciiMode" rawSetAsciiMode :: Ptr Writer -> CBool -> IO ()

setAsciiMode :: Ptr Writer -> Bool -> IO ()
setAsciiMode writer mode = rawSetAsciiMode writer (boolToCBool mode)


foreign import capi unsafe "hs_StlAPI_Writer.h hs_StlAPI_Writer_write" rawWrite :: Ptr Writer -> Ptr TopoDS.Shape -> CString -> IO (CBool)

write :: Ptr Writer -> Ptr TopoDS.Shape -> String -> IO (Bool)
write writer shape filename = cBoolToBool <$> withCString filename (rawWrite writer shape)