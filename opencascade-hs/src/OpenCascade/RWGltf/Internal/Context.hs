module OpenCascade.RWGltf.Internal.Context
( rwGltfContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.RWGltf.Types

rwGltfContext :: C.Context
rwGltfContext = C.cppTypePairs rwGltfTypePairs
  where
    rwGltfTypePairs :: [(CIdentifier, Q Type)]
    rwGltfTypePairs =
      [ ("RWGltf_CafWriter", [t| CafWriter |])
      , ("RWGltf_CafReader", [t| CafReader |])
      ]
