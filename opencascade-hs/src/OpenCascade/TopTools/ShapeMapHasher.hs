{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopTools.ShapeMapHasher 
( hash
, isEqual
) where

import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_TopTools_ShapeMapHasher.h hs_TopTools_ShapeMapHasher_hash" rawHash 
    :: Ptr TopoDS.Shape 
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CInt)

hash :: Ptr TopoDS.Shape -> IO Int
hash s = fromIntegral <$> wrapException (rawHash s)

foreign import capi unsafe "hs_TopTools_ShapeMapHasher.h hs_TopTools_ShapeMapHasher_isEqual" rawIsEqual 
    :: Ptr TopoDS.Shape 
    -> Ptr TopoDS.Shape 
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CBool)

isEqual :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> IO Bool
isEqual a b = cBoolToBool <$> wrapException (rawIsEqual a b)