module OpenCascade.RWObj.Internal.Context
( rwObjContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.RWObj.Types

rwObjContext :: C.Context
rwObjContext = C.cppTypePairs rwObjTypePairs
  where
    rwObjTypePairs :: [(CIdentifier, Q Type)]
    rwObjTypePairs =
      [ ("RWObj_CafWriter", [t| CafWriter |])
      , ("RWObj_CafReader", [t| CafReader |])
      ]
