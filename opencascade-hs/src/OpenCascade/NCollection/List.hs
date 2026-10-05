{-# LANGUAGE CApiFFI #-}
-- | Bindings to @NCollection_List@.
--
-- Currently only instantiated for @TopoDS_Shape@
-- (the type that was historically spelled @TopTools_ListOfShape@).
module OpenCascade.NCollection.List
( List
, newListOfShape
, extentListOfShape
, appendListOfShape
, valueListOfShape
, fromListOfShape
) where

import OpenCascade.NCollection.Types (List)
import OpenCascade.NCollection.Internal.Destructors (deleteListOfShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
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

foreign import capi unsafe "hs_NCollection_List.h hs_NCollection_List_TopoDS_Shape_value" rawValueListOfShape
    :: Ptr (List TopoDS.Shape)
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

-- | The element at a (0-based) index.
--
-- Throws @Standard_OutOfRange@ if the index is out of bounds.
--
-- The underlying list has no indexed access, so this is \(O(n)\).
valueListOfShape :: Ptr (List TopoDS.Shape) -> Int -> Acquire (Ptr TopoDS.Shape)
valueListOfShape list index = mkAcquire (wrapException $ rawValueListOfShape list (fromIntegral index)) deleteShape

-- | Read every element of the list into a Haskell list.
--
-- Each element is a fresh copy of the @TopoDS_Shape@ handle,
-- so the result outlives the @NCollection_List@ it was read from.
fromListOfShape :: Ptr (List TopoDS.Shape) -> Acquire [Ptr TopoDS.Shape]
fromListOfShape list = do
    n <- liftIO $ extentListOfShape list
    traverse (valueListOfShape list) [0 .. n - 1]
