{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.Std.Types
( Exception
, RuntimeError
) where

import OpenCascade.Inheritance

data Exception
data RuntimeError

instance SubTypeOf Exception RuntimeError
