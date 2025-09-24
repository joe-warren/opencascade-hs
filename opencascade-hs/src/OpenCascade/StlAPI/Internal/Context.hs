module OpenCascade.StlAPI.Internal.Context
( stlAPIContext
) where

import qualified Language.C.Inline.Cpp as C
import Language.C.Types (CIdentifier)
import Language.Haskell.TH (Q, Type)
import OpenCascade.StlAPI.Types

stlAPIContext :: C.Context
stlAPIContext = C.cppTypePairs stlAPITypePairs
  where
    stlAPITypePairs :: [(CIdentifier, Q Type)]
    stlAPITypePairs =
      [ ("StlAPI_Writer", [t| Writer |])
      , ("StlAPI_Reader", [t| Reader |])
      ]
