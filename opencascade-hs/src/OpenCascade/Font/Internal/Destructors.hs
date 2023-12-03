{-# LANGUAGE CApiFFI #-}
module OpenCascade.Font.Internal.Destructors where


import OpenCascade.Font.Types

import Foreign.Ptr

foreign import capi unsafe "hs_Font_BRepFont.h hs_delete_Font_BRepFont" deleteBRepFont :: Ptr BRepFont -> IO ()

foreign import capi unsafe "hs_Font_BRepTextBuilder.h hs_delete_Font_BRepTextBuilder" deleteBRepTextBuilder :: Ptr BRepTextBuilder -> IO ()