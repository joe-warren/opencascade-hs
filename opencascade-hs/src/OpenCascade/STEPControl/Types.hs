{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.STEPControl.Types
( Writer
, Reader
) where

import OpenCascade.Inheritance
import qualified OpenCascade.XSControl.Types as XSControl
data Writer
data Reader

instance SubTypeOf XSControl.Reader Reader