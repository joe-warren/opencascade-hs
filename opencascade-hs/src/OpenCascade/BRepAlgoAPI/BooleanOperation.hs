{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.BooleanOperation 
( BooleanOperation
, setTools
) where

import OpenCascade.BRepAlgoAPI.Types (BooleanOperation)
import Foreign.C (CInt (..))
import Foreign.Ptr (Ptr)
import OpenCascade.Internal.Exception(wrapException)
import qualified OpenCascade.NCollection as NCollection
import qualified OpenCascade.TopoDS as TopoDS


foreign import capi unsafe "hs_BRepAlgoAPI_BooleanOperation.h hs_BRepAlgoAPI_BooleanOperation_setTools" rawSetTools
    :: Ptr BooleanOperation
    -> Ptr (NCollection.List TopoDS.Shape)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setTools :: Ptr BooleanOperation -> Ptr (NCollection.List TopoDS.Shape) -> IO ()
setTools builder arguments = wrapException (rawSetTools builder arguments)
