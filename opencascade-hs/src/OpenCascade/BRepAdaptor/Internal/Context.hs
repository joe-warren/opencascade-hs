module OpenCascade.BRepAdaptor.Internal.Context
( brepAdaptorContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepAdaptor.Types

brepAdaptorContext :: C.Context
brepAdaptorContext = C.cppTypePairs brepAdaptorTypePairs
  where
    brepAdaptorTypePairs :: [(CIdentifier, Q Type)]
    brepAdaptorTypePairs =
      [ ("BRepAdaptor_Curve", [t| Curve |])
      ]
