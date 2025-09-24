module OpenCascade.Bnd.Internal.Context
( bndContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.Bnd.Types

bndContext :: C.Context
bndContext = C.cppTypePairs bndTypePairs
  where
    bndTypePairs :: [(CIdentifier, Q Type)]
    bndTypePairs =
      [ ("Bnd_Box", [t| Box |])
      , ("Bnd_OBB", [t| OBB |])
      ]
