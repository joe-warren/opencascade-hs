module OpenCascade.TopLoc.Internal.Context
( topLocContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.TopLoc.Types

topLocContext :: C.Context
topLocContext = C.cppTypePairs topLocTypePairs
  where
    topLocTypePairs :: [(CIdentifier, Q Type)]
    topLocTypePairs =
      [ ("TopLoc_Location", [t| Location |])
      ]
