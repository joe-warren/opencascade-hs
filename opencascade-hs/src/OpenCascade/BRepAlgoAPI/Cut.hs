{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Cut
( Cut
, new
, fromShapes
) where

import OpenCascade.BRepAlgoAPI.Types (Cut)
import OpenCascade.BRepAlgoAPI.Internal.Destructors (deleteCut)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepAlgoAPI_Cut.h hs_new_BRepAlgoAPI_Cut" rawNew
    :: IO (Ptr Cut)

new :: Acquire (Ptr Cut)
new = mkAcquire rawNew deleteCut

foreign import capi unsafe "hs_BRepAlgoAPI_Cut.h hs_new_BRepAlgoAPI_Cut_fromShapes" rawFromShapes
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Cut)

fromShapes :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr Cut)
fromShapes a b = mkAcquire (wrapException $ rawFromShapes a b) deleteCut
