module OpenCascade.TDocStd.Internal.Context
( tDocStdContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.TDocStd.Types

tDocStdContext :: C.Context
tDocStdContext = C.cppTypePairs tDocStdTypePairs
  where
    tDocStdTypePairs :: [(CIdentifier, Q Type)]
    tDocStdTypePairs =
      [ ("TDocStd_Document", [t| Document |])
      ]
