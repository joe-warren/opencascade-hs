{-# LANGUAGE CApiFFI #-}
module Lib
    ( someFunc
    ) where
import Foreign.C

foreign import capi unsafe "bottle.h saveBottle" saveBottle :: CDouble -> CDouble -> CDouble -> CString -> IO CInt

someFunc :: IO ()
someFunc = withCString "haskell.stl" (saveBottle 50 50 50) >>= print
