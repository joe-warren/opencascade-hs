module OpenCascade.BRepOffsetAPI.Internal.Context
( brepOffsetAPIContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BRepOffsetAPI.Types

brepOffsetAPIContext :: C.Context
brepOffsetAPIContext = C.cppTypePairs brepOffsetAPITypePairs
  where
    brepOffsetAPITypePairs :: [(CIdentifier, Q Type)]
    brepOffsetAPITypePairs =
      [ ("BRepOffsetAPI_MakePipe", [t| MakePipe |])
      , ("BRepOffsetAPI_MakeOffsetShape", [t| MakeOffsetShape |])
      , ("BRepOffsetAPI_ThruSections", [t| ThruSections |])
      ]
