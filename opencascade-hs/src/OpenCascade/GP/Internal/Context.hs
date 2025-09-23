module OpenCascade.GP.Internal.Context
( gpContext
) where

import OpenCascade.GP.Types
import qualified Language.C.Inline.Cpp as C

-- | Shared C++ type pairs context for all OpenCascade.GP modules
-- This contains only the type mappings for all common GP types
gpContext :: C.Context
gpContext = C.cppTypePairs [
  ("gp_Pnt", [t|Pnt|]),
  ("gp_Pnt2d", [t|Pnt2d|]),
  ("gp_Ax1", [t|Ax1|]),
  ("gp_Ax2", [t|Ax2|]),
  ("gp_Ax2d", [t|Ax2d|]),
  ("gp_Ax3", [t|Ax3|]),
  ("gp_Dir", [t|Dir|]),
  ("gp_Dir2d", [t|Dir2d|]),
  ("gp_Vec", [t|Vec|]),
  ("gp_Vec2d", [t|Vec2d|]),
  ("gp_Trsf", [t|Trsf|]),
  ("gp_Trsf2d", [t|Trsf2d|]),
  ("gp_GTrsf", [t|GTrsf|]),
  ("gp_XYZ", [t|XYZ|])
  ]
