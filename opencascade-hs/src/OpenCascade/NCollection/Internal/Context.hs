module OpenCascade.NCollection.Internal.Context
( nCollectionContext
) where

import OpenCascade.NCollection.Types
import qualified Language.C.Inline.Cpp as C

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

nCollectionContext :: C.Context
nCollectionContext = C.cppTypePairs [
  ("NCollection_Array1", [t|Array1|])
  ]