{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.IndexedDataMap
( newAsciiStringMap
) where

import OpenCascade.NCollection.Types (IndexedDataMap)
import OpenCascade.NCollection.Internal.Destructors (deleteAsciiStringMap)
import qualified OpenCascade.TCollection.Types as TCollection
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_new_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString" rawNewAsciiStringMap
    :: IO (Ptr (IndexedDataMap TCollection.AsciiString TCollection.AsciiString))

newAsciiStringMap :: Acquire (Ptr (IndexedDataMap TCollection.AsciiString TCollection.AsciiString))
newAsciiStringMap = mkAcquire rawNewAsciiStringMap deleteAsciiStringMap
