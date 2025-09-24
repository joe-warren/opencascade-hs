module OpenCascade.Font.Internal.Context
( fontContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.Font.Types

fontContext :: C.Context
fontContext = C.cppTypePairs fontTypePairs
  where
    fontTypePairs :: [(CIdentifier, Q Type)]
    fontTypePairs =
      [ ("Font_BRepFont", [t| BRepFont |])
      , ("Font_BRepTextBuilder", [t| BRepTextBuilder |])
      ]
