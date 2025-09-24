module OpenCascade.TColStd.Internal.Context
( tColStdContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.TColStd.Types

tColStdContext :: C.Context
tColStdContext = C.cppTypePairs tColStdTypePairs
  where
    tColStdTypePairs :: [(CIdentifier, Q Type)]
    tColStdTypePairs =
      [ ("TColStd_IndexedDataMapOfStringString", [t| IndexedDataMapOfStringString |])
      ]
