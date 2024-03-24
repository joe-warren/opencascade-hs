{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWObj.CafReader 
( CafReader
, new
, setSinglePrecision
) where

import OpenCascade.RWObj.Types (CafReader)
import OpenCascade.RWObj.Internal.Destructors (deleteCafReader)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import Foreign.C (CBool (..))
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_RWObj_CafReader.h hs_new_RWObj_CafReader" rawNew :: IO (Ptr CafReader)

new :: Acquire (Ptr CafReader)
new = mkAcquire rawNew deleteCafReader


foreign import capi unsafe "hs_RWObj_CafReader.h hs_RWObj_CafReader_setSinglePrecision" rawSetSinglePrecision :: Ptr CafReader -> CBool -> IO ()

setSinglePrecision :: Ptr CafReader -> Bool -> IO ()
setSinglePrecision reader isSingle = rawSetSinglePrecision reader (boolToCBool isSingle)