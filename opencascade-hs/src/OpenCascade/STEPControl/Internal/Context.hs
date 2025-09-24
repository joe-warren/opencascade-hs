module OpenCascade.STEPControl.Internal.Context
( stepControlContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.STEPControl.Types

stepControlContext :: C.Context
stepControlContext = C.cppTypePairs stepControlTypePairs
  where
    stepControlTypePairs :: [(CIdentifier, Q Type)]
    stepControlTypePairs =
      [ ("STEPControl_Writer", [t| Writer |])
      , ("STEPControl_Reader", [t| Reader |])
      ]
