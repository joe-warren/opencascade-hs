{-# LANGUAGE CApiFFI #-} 
module OpenCascade.BOPAlgo.Builder
( Builder
, new
, addArgument
, setRunParallel
, shape
) where

import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteBuilder)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_new_BOPAlgo_Builder" rawNew :: IO (Ptr Builder)

new :: Acquire (Ptr Builder)
new = mkAcquire rawNew deleteBuilder

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_AddArgument" addArgument :: Ptr Builder -> Ptr TopoDS.Shape -> IO ()

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_SetRunParallel" rawSetRunParallel :: Ptr Builder -> CBool -> IO ()

setRunParallel :: Ptr Builder -> Bool -> IO ()
setRunParallel builder = rawSetRunParallel builder . boolToCBool

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_Shape" rawShape :: Ptr Builder -> IO (Ptr TopoDS.Shape)

shape :: Ptr Builder -> Acquire (Ptr TopoDS.Shape)
shape builder = mkAcquire (rawShape builder) deleteShape  

