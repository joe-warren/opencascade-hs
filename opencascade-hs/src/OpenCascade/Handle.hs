{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.Handle 
( Handle
) where
import OpenCascade.Inheritance

data Handle a 

instance SubTypeOf a b => SubTypeOf (Handle a) (Handle b)




