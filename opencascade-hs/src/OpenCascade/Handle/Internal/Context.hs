module OpenCascade.Handle.Internal.Context
( handleContext
) where

import OpenCascade.Handle
import qualified Language.C.Inline.Cpp as C

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

handleContext :: C.Context
handleContext = C.cppTypePairs [
  ("opencascade::handle", [t|Handle|])
  ]