{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopTools.ListOfShape
( ListOfShape
, new
, extent
, append
, value
) where

import OpenCascade.TopTools.Types (ListOfShape)
import OpenCascade.TopTools.Internal.Destructors (deleteListOfShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_TopTools_ListOfShape.h hs_new_TopTools_ListOfShape" rawNew :: IO (Ptr ListOfShape)

new :: Acquire (Ptr ListOfShape)
new = mkAcquire rawNew deleteListOfShape

foreign import capi unsafe "hs_TopTools_ListOfShape.h hs_TopTools_ListOfShape_extent" rawExtent :: Ptr ListOfShape -> IO CInt

extent :: Ptr ListOfShape -> IO Int
extent = fmap fromIntegral . rawExtent

foreign import capi unsafe "hs_TopTools_ListOfShape.h hs_TopTools_ListOfShape_append" rawAppend :: Ptr ListOfShape -> Ptr TopoDS.Shape -> IO ()

append :: Ptr ListOfShape -> Ptr TopoDS.Shape -> IO ()
append = rawAppend

foreign import capi unsafe "hs_TopTools_ListOfShape.h hs_TopTools_ListOfShape_value" rawValue
    :: Ptr ListOfShape
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

-- index is 0 based
value :: Ptr ListOfShape -> Int -> Acquire (Ptr TopoDS.Shape)
value list index = mkAcquire (wrapException $ rawValue list (fromIntegral index)) deleteShape
