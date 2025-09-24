module OpenCascade.Message.Internal.Context
( messageContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.Message.Types

messageContext :: C.Context
messageContext = C.cppTypePairs messageTypePairs
  where
    messageTypePairs :: [(CIdentifier, Q Type)]
    messageTypePairs =
      [ ("Message_ProgressRange", [t| ProgressRange |])
      ]
