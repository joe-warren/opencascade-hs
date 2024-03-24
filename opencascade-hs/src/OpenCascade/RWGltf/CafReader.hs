{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWGltf.CafReader 
( CafReader
, new
, setDoublePrecision
) where

import OpenCascade.RWGltf.Types (CafReader)
import OpenCascade.RWGltf.Internal.Destructors (deleteCafReader)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import Foreign.C (CBool (..))
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_RWGltf_CafReader.h hs_new_RWGltf_CafReader" rawNew :: IO (Ptr CafReader)

new :: Acquire (Ptr CafReader)
new = mkAcquire rawNew deleteCafReader


foreign import capi unsafe "hs_RWGltf_CafReader.h hs_RWGltf_CafReader_setDoublePrecision" rawSetDoublePrecision :: Ptr CafReader -> CBool -> IO ()

setDoublePrecision :: Ptr CafReader -> Bool -> IO ()
setDoublePrecision reader isDouble = rawSetDoublePrecision reader (boolToCBool isDouble)