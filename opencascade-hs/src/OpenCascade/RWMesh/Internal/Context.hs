module OpenCascade.RWMesh.Internal.Context
( rwMeshContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.RWMesh.Types

rwMeshContext :: C.Context
rwMeshContext = C.cppTypePairs rwMeshTypePairs
  where
    rwMeshTypePairs :: [(CIdentifier, Q Type)]
    rwMeshTypePairs =
      [ ("RWMesh_CafReader", [t| CafReader |])
      ]
