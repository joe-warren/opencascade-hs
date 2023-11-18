{-# LANGUAGE MultiParamTypeClasses #-} 

module OpenCascade.Inheritance 
( SubTypeOf (..)
, DiscriminatedSubTypeOf (..)
, unsafeDowncast
) where

import Foreign.Ptr

class SubTypeOf a b where
    upcast :: Ptr b -> Ptr a
    upcast = castPtr

class SubTypeOf a b => DiscriminatedSubTypeOf a b where
    downcast :: Ptr a -> IO (Maybe (Ptr b))

unsafeDowncast :: DiscriminatedSubTypeOf a b => Ptr a -> IO (Ptr b)
unsafeDowncast p = do
    maybeT <- downcast p
    maybe (error "Incorrect subtype in cast") pure maybeT
    
    
