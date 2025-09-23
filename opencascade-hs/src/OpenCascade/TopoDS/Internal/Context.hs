module OpenCascade.TopoDS.Internal.Context
( topoDSContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.TopoDS.Types
import OpenCascade.TopLoc.Internal.Context (topLocContext)

topoDSContext :: C.Context
topoDSContext = topLocContext <> C.cppTypePairs topoDSTypePairs
  where
    topoDSTypePairs :: [(CIdentifier, Q Type)]
    topoDSTypePairs =
      [ ("TopoDS_Shape", [t| Shape |])
      , ("TopoDS_Vertex", [t| Vertex |])
      , ("TopoDS_Edge", [t| Edge |])
      , ("TopoDS_Face", [t| Face |])
      , ("TopoDS_Wire", [t| Wire |])
      , ("TopoDS_Shell", [t| Shell |])
      , ("TopoDS_Solid", [t| Solid |])
      , ("TopoDS_Compound", [t| Compound |])
      , ("TopoDS_CompSolid", [t| CompSolid |])
      , ("TopoDS_Builder", [t| Builder |])
      ]
