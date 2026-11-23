{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.BuilderAlgo
( BuilderAlgo
, setArguments
, setNonDestructive
) where

import OpenCascade.BRepAlgoAPI.Types (BuilderAlgo)
import Foreign.C (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception(wrapException)
import qualified OpenCascade.NCollection as NCollection
import qualified OpenCascade.TopoDS as TopoDS


foreign import capi unsafe "hs_BRepAlgoAPI_BuilderAlgo.h hs_BRepAlgoAPI_BuilderAlgo_setNonDestructive" rawSetNonDestructive
    :: Ptr BuilderAlgo
    -> CBool
    -> IO ()

setNonDestructive :: Ptr BuilderAlgo -> Bool -> IO ()
setNonDestructive builder nonDestructive = rawSetNonDestructive builder (boolToCBool nonDestructive)

foreign import capi unsafe "hs_BRepAlgoAPI_BuilderAlgo.h hs_BRepAlgoAPI_BuilderAlgo_setArguments" rawSetArguments
    :: Ptr BuilderAlgo
    -> Ptr (NCollection.List TopoDS.Shape)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setArguments :: Ptr BuilderAlgo -> Ptr (NCollection.List TopoDS.Shape) -> IO ()
setArguments builder arguments = wrapException (rawSetArguments builder arguments)
