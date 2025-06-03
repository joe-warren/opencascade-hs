{-# LANGUAGE CApiFFI #-} 
module OpenCascade.BOPAlgo.BOP 
( BOP
, new
, setOperation
) where

import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteBOP)
import OpenCascade.BOPAlgo.Operation (Operation)

import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C (CInt (..))


foreign import capi unsafe "hs_BOPAlgo_BOP.h hs_new_BOPAlgo_BOP" rawNew :: IO (Ptr BOP)

new :: Acquire (Ptr BOP)
new = mkAcquire rawNew deleteBOP


foreign import capi unsafe "hs_BOPAlgo_BOP.h hs_BOPAlgo_BOP_SetOperation" rawSetOperation :: Ptr BOP -> CInt -> IO ()

setOperation :: Ptr BOP -> Operation -> IO ()
setOperation bop op = rawSetOperation bop (fromIntegral . fromEnum $ op)