module OpenCascade.BRepFilletAPI.Internal.Context
( brepFilletAPIContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepFilletAPI.Types

brepFilletAPIContext :: C.Context
brepFilletAPIContext = C.cppTypePairs brepFilletAPITypePairs
  where
    brepFilletAPITypePairs :: [(CIdentifier, Q Type)]
    brepFilletAPITypePairs =
      [ ("BRepFilletAPI_MakeFillet", [t| MakeFillet |])
      , ("BRepFilletAPI_MakeChamfer", [t| MakeChamfer |])
      ]
