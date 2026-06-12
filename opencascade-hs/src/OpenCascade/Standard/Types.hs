{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.Standard.Types 
(Failure
) where

import OpenCascade.Std.Types (Exception)
import OpenCascade.Inheritance (SubTypeOf)

data Failure

instance SubTypeOf Exception Failure