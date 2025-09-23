module OpenCascade.BRepPrimAPI.Internal.Context
( brepPrimAPIContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepPrimAPI.Types

brepPrimAPIContext :: C.Context
brepPrimAPIContext = C.cppTypePairs brepPrimAPITypePairs
  where
    brepPrimAPITypePairs :: [(CIdentifier, Q Type)]
    brepPrimAPITypePairs =
      [ ("BRepPrimAPI_MakeBox", [t| MakeBox |])
      , ("BRepPrimAPI_MakeRevol", [t| MakeRevol |])
      , ("BRepPrimAPI_MakeSphere", [t| MakeSphere |])
      , ("BRepPrimAPI_MakeCylinder", [t| MakeCylinder |])
      , ("BRepPrimAPI_MakeCone", [t| MakeCone |])
      , ("BRepPrimAPI_MakePrism", [t| MakePrism |])
      ]
