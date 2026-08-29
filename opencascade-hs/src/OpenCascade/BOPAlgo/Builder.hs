{-# LANGUAGE CApiFFI #-} 
module OpenCascade.BOPAlgo.Builder
( Builder
, new
, addArgument
, setRunParallel
, shape
, perform
, modified
, generated
, isDeleted
) where

import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteBuilder)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.TopTools.Types as TopTools
import OpenCascade.TopTools.Internal.Destructors (deleteListOfShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)

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

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_Modified" rawModified
    :: Ptr Builder
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopTools.ListOfShape)

modified :: Ptr Builder -> Ptr TopoDS.Shape -> Acquire (Ptr TopTools.ListOfShape)
modified builder shape' = mkAcquire (wrapException $ rawModified builder shape') deleteListOfShape

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_Generated" rawGenerated
    :: Ptr Builder
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopTools.ListOfShape)

generated :: Ptr Builder -> Ptr TopoDS.Shape -> Acquire (Ptr TopTools.ListOfShape)
generated builder shape' = mkAcquire (wrapException $ rawGenerated builder shape') deleteListOfShape

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_BOPAlgo_Builder_IsDeleted" rawIsDeleted
    :: Ptr Builder
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

isDeleted :: Ptr Builder -> Ptr TopoDS.Shape -> IO Bool
isDeleted builder shape' = cBoolToBool <$> wrapException (rawIsDeleted builder shape')

