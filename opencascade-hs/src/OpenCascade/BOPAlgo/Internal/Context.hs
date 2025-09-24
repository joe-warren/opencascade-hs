module OpenCascade.BOPAlgo.Internal.Context
( bopAlgoContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.BOPAlgo.Types

bopAlgoContext :: C.Context
bopAlgoContext = C.cppTypePairs bopAlgoTypePairs
  where
    bopAlgoTypePairs :: [(CIdentifier, Q Type)]
    bopAlgoTypePairs =
      [ ("BOPAlgo_Builder", [t| Builder |])
      , ("BOPAlgo_BOP", [t| BOP |])
      ]
