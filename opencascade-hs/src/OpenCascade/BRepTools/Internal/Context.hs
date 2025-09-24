module OpenCascade.BRepTools.Internal.Context
( brepToolsContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepTools.Types

brepToolsContext :: C.Context
brepToolsContext = C.cppTypePairs brepToolsTypePairs
  where
    brepToolsTypePairs :: [(CIdentifier, Q Type)]
    brepToolsTypePairs =
      [ ("BRepTools_WireExplorer", [t| WireExplorer |])
      ]
