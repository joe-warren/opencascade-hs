{-# LANGUAGE CApiFFI #-} 
module OpenCascade.BOPAlgo.Builder
( Builder
, new
, addArgument
, setRunParallel
, shape
, perform
) where

import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteBuilder)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_new_BOPAlgo_Builder" rawNew :: IO (Ptr Builder)

new :: Acquire (Ptr Builder)
new = mkAcquire rawNew deleteBuilder

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_AddArgument" rawAddArgument
    :: Ptr Builder
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addArgument :: Ptr Builder -> Ptr TopoDS.Shape -> IO ()
addArgument builder shape' = wrapException $ rawAddArgument builder shape'

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_SetRunParallel" rawSetRunParallel :: Ptr Builder -> CBool -> IO ()

setRunParallel :: Ptr Builder -> Bool -> IO ()
setRunParallel builder = rawSetRunParallel builder . boolToCBool

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_Shape" rawShape
    :: Ptr Builder
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

shape :: Ptr Builder -> Acquire (Ptr TopoDS.Shape)
shape builder = mkAcquire (wrapException $ rawShape builder) deleteShape


foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_Perform" rawPerform
    :: Ptr Builder
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

perform :: Ptr Builder -> IO ()
perform builder = wrapException $ rawPerform builder

