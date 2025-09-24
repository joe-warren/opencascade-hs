module OpenCascade.Poly.Internal.Context
( polyContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.Poly.Types

polyContext :: C.Context
polyContext = C.cppTypePairs polyTypePairs
  where
    polyTypePairs :: [(CIdentifier, Q Type)]
    polyTypePairs =
      [ ("Poly_Triangle", [t| Triangle |])
      , ("Poly_Triangulation", [t| Triangulation |])
      ]
