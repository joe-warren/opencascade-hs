{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BOPAlgo.Types
( Builder
, BOP
) where

import qualified OpenCascade.Inheritance as Inheritance

data Builder
data BOP

instance Inheritance.SubTypeOf Builder BOP
