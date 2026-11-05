{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Section
( Section
, fromShapes
) where

import OpenCascade.BRepAlgoAPI.Types (Section)
import OpenCascade.BRepAlgoAPI.Internal.Destructors (deleteSection)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepAlgoAPI_Section.h hs_new_BRepAlgoAPI_Section_fromShapes" rawFromShapes
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Section)

fromShapes :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr Section)
fromShapes a b = mkAcquire (wrapException $ rawFromShapes a b) deleteSection
