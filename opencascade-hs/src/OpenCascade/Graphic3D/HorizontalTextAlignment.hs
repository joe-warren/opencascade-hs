module OpenCascade.Graphic3D.HorizontalTextAlignment
( HorizontalTextAlignment (..)
) where

import Prelude hiding (Either (..))

-- Should match the order in Graphic3D.HorizontalTextAlignment.hxx
data HorizontalTextAlignment = Left | Center | Right deriving (Enum, Show)