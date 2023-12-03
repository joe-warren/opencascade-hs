{-# LANGUAGE  CApiFFI #-}
module OpenCascade.Font.BRepFont 
( BRepFont
, fromPathAndSize
, ascender
, descender
, lineSpacing
, pointSize
) where

import OpenCascade.Font.Types (BRepFont)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C
import Foreign.Ptr
import OpenCascade.Font.Internal.Destructors (deleteBRepFont)
import Data.Coerce


foreign import capi unsafe "hs_Font_BRepFont.h hs_new_Font_BRepFont_fromStringAndSize" rawFromPathAndSize :: CString -> CDouble -> IO (Ptr BRepFont)

fromPathAndSize :: FilePath -> Double -> Acquire (Ptr BRepFont)
fromPathAndSize path size = mkAcquire (withCString path $ \str -> rawFromPathAndSize str (coerce size)) deleteBRepFont

foreign import capi unsafe "hs_Font_BRepFont.h hs_Font_BRepFont_ascender" rawAscender :: Ptr BRepFont -> IO CDouble

ascender :: Ptr BRepFont -> IO Double
ascender = coerce rawAscender

foreign import capi unsafe "hs_Font_BRepFont.h hs_Font_BRepFont_descender" rawDescender :: Ptr BRepFont -> IO CDouble

descender :: Ptr BRepFont -> IO Double
descender = coerce rawDescender

foreign import capi unsafe "hs_Font_BRepFont.h hs_Font_BRepFont_lineSpacing" rawLineSpacing :: Ptr BRepFont -> IO CDouble

lineSpacing :: Ptr BRepFont -> IO Double
lineSpacing = coerce rawLineSpacing

foreign import capi unsafe "hs_Font_BRepFont.h hs_Font_BRepFont_pointSize" rawPointSize :: Ptr BRepFont -> IO CDouble

pointSize :: Ptr BRepFont -> IO Double
pointSize = coerce rawPointSize