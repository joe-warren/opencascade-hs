module OpenCascade.Geom.Internal.Context
( geomContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import qualified OpenCascade.Geom as Geom

geomContext :: C.Context
geomContext = C.cppTypePairs geomTypePairs
  where
    geomTypePairs :: [(CIdentifier, Q Type)]
    geomTypePairs =
      [ ("Geom_Curve", [t| Geom.Curve |])
      , ("Geom_Surface", [t| Geom.Surface |])
      , ("Geom_BSplineCurve", [t| Geom.BSplineCurve |])
      , ("Geom_BezierCurve", [t| Geom.BezierCurve |])
      ]
