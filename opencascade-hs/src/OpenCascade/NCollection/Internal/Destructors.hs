{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.Internal.Destructors
( deletePntArray
, deleteAsciiStringMap
) where

import OpenCascade.NCollection.Types
import OpenCascade.GP.Types
import qualified OpenCascade.TCollection.Types as TCollection
import Foreign.Ptr

foreign import capi unsafe "hs_NCollection_Array1.h hs_delete_NCollection_Array1_gp_Pnt" deletePntArray :: Ptr (Array1 Pnt) -> IO ()

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_delete_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString" deleteAsciiStringMap :: Ptr (IndexedDataMap TCollection.AsciiString TCollection.AsciiString) -> IO ()

