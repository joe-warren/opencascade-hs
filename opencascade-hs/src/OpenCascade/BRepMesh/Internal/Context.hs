module OpenCascade.BRepMesh.Internal.Context
( brepMeshContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepMesh.Types

brepMeshContext :: C.Context
brepMeshContext = C.cppTypePairs brepMeshTypePairs
  where
    brepMeshTypePairs :: [(CIdentifier, Q Type)]
    brepMeshTypePairs =
      [ ("BRepMesh_IncrementalMesh", [t| IncrementalMesh |])
      ]
