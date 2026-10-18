{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.List
( List
, newListOfShape
, extentListOfShape
, appendListOfShape
) where

import OpenCascade.NCollection.Types (List)
import OpenCascade.NCollection.Internal.Destructors (deleteListOfShape)
import OpenCascade.NCollection.List.Iterator (newListOfShapeIterator, moreListOfShapeIterator, nextListOfShapeIterator, valueListOfShapeIterator)
import qualified OpenCascade.TopoDS.Types as TopoDS
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_NCollection_List.h hs_new_NCollection_List_TopoDS_Shape" rawNewListOfShape :: IO (Ptr (List TopoDS.Shape))

newListOfShape :: Acquire (Ptr (List TopoDS.Shape))
newListOfShape = mkAcquire rawNewListOfShape deleteListOfShape

foreign import capi unsafe "hs_NCollection_List.h hs_NCollection_List_TopoDS_Shape_extent" rawExtentListOfShape :: Ptr (List TopoDS.Shape) -> IO CInt

extentListOfShape :: Ptr (List TopoDS.Shape) -> IO Int
extentListOfShape = fmap fromIntegral . rawExtentListOfShape

foreign import capi unsafe "hs_NCollection_List.h hs_NCollection_List_TopoDS_Shape_append" appendListOfShape :: Ptr (List TopoDS.Shape) -> Ptr TopoDS.Shape -> IO ()

