module OpenCascade.BRepBuilderAPI.Internal.Context
( brepBuilderAPIContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepBuilderAPI.Types

brepBuilderAPIContext :: C.Context
brepBuilderAPIContext = C.cppTypePairs brepBuilderAPITypePairs
  where
    brepBuilderAPITypePairs :: [(CIdentifier, Q Type)]
    brepBuilderAPITypePairs =
      [ ("BRepBuilderAPI_MakeVertex", [t| MakeVertex |])
      , ("BRepBuilderAPI_MakeWire", [t| MakeWire |])
      , ("BRepBuilderAPI_MakeFace", [t| MakeFace |])
      , ("BRepBuilderAPI_MakeSolid", [t| MakeSolid |])
      , ("BRepBuilderAPI_MakeShape", [t| MakeShape |])
      , ("BRepBuilderAPI_Sewing", [t| Sewing |])
      ]
