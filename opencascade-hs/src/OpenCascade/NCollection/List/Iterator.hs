{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.List.Iterator
( ListIterator
, newListOfShapeIterator
, moreListOfShapeIterator
, nextListOfShapeIterator
, valueListOfShapeIterator
) where

import OpenCascade.NCollection.Types (List, ListIterator)
import OpenCascade.NCollection.Internal.Destructors (deleteListOfShapeIterator)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_NCollection_List.h hs_new_NCollection_List_TopoDS_Shape_Iterator" rawNewListOfShapeIterator :: Ptr (List TopoDS.Shape) -> IO (Ptr (ListIterator TopoDS.Shape))

newListOfShapeIterator :: Ptr (List TopoDS.Shape) -> Acquire (Ptr (ListIterator TopoDS.Shape))
newListOfShapeIterator list = mkAcquire (rawNewListOfShapeIterator list) deleteListOfShapeIterator

foreign import capi unsafe "hs_NCollection_List.h hs_NCollection_List_TopoDS_Shape_Iterator_more" rawMoreListOfShapeIterator :: Ptr (ListIterator TopoDS.Shape) -> IO CBool

moreListOfShapeIterator :: Ptr (ListIterator TopoDS.Shape) -> IO Bool
moreListOfShapeIterator = fmap cBoolToBool . rawMoreListOfShapeIterator

foreign import capi unsafe "hs_NCollection_List.h hs_NCollection_List_TopoDS_Shape_Iterator_next" nextListOfShapeIterator :: Ptr (ListIterator TopoDS.Shape) -> IO ()

foreign import capi unsafe "hs_NCollection_List.h hs_NCollection_List_TopoDS_Shape_Iterator_value" rawValueListOfShapeIterator :: Ptr (ListIterator TopoDS.Shape) -> IO (Ptr TopoDS.Shape)

valueListOfShapeIterator :: Ptr (ListIterator TopoDS.Shape) -> Acquire (Ptr TopoDS.Shape)
valueListOfShapeIterator iterator = mkAcquire (rawValueListOfShapeIterator iterator) deleteShape
