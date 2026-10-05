{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Common
( Common
, fromShapes
) where

import OpenCascade.BRepAlgoAPI.Types (Common)
import OpenCascade.BRepAlgoAPI.Internal.Destructors (deleteCommon)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepAlgoAPI_Common.h hs_new_BRepAlgoAPI_Common_fromShapes" rawFromShapes
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Common)

fromShapes :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr Common)
fromShapes a b = mkAcquire (wrapException $ rawFromShapes a b) deleteCommon
