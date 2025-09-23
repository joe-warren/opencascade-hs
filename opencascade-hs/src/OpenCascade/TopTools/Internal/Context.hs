module OpenCascade.TopTools.Internal.Context
( topToolsContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import qualified OpenCascade.TopTools as TopTools

topToolsContext :: C.Context
topToolsContext = C.cppTypePairs topToolsTypePairs
  where
    topToolsTypePairs :: [(CIdentifier, Q Type)]
    topToolsTypePairs =
      [ ("TopTools_ListOfShape", [t| TopTools.ListOfShape |])
      ]
