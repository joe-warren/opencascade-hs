{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}
module Lib
    ( someFunc
    ) where
import Foreign.C
import Foreign.Ptr

data Shape

foreign import capi unsafe "bottle.h SaveShapeSTL" saveShapeSTL :: CDouble -> Ptr Shape -> CString -> IO CInt
foreign import capi unsafe "bottle.h MakeBottle" makeBottle :: CDouble -> CDouble -> CDouble -> IO (Ptr Shape)

someFunc :: IO ()
someFunc = do
    shape <- makeBottle 50 100 50
    withCString "haskell.stl" (saveShapeSTL 0.01 shape) >>= print
