{-# LANGUAGE  CApiFFI #-}
module OpenCascade.Font.BRepFont 
( BRepFont
, new
, fromPathAndSize
, initFromPathAndSize
, initFromNameAspectAndSize
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
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Font.FontAspect (FontAspect)


foreign import capi unsafe "hs_Font_BRepFont.h hs_new_Font_BRepFont" rawNew :: IO (Ptr BRepFont)

new :: Acquire (Ptr BRepFont)
new  = mkAcquire rawNew deleteBRepFont

foreign import capi unsafe "hs_Font_BRepFont.h hs_new_Font_BRepFont_fromStringAndSize" rawFromPathAndSize :: CString -> CDouble -> IO (Ptr BRepFont)

fromPathAndSize :: FilePath -> Double -> Acquire (Ptr BRepFont)
fromPathAndSize path size = mkAcquire (withCString path $ \str -> rawFromPathAndSize str (coerce size)) deleteBRepFont

foreign import capi unsafe "hs_Font_BRepFont.h hs_Font_BRepFont_initPathAndSize" rawInitFromPathAndSize :: Ptr BRepFont ->  CString -> CDouble -> IO CBool

initFromPathAndSize :: Ptr BRepFont -> FilePath -> Double -> IO Bool
initFromPathAndSize font fontPath size = withCString fontPath $ \str -> cBoolToBool <$> rawInitFromPathAndSize font str (coerce size)

foreign import capi unsafe "hs_Font_BRepFont.h hs_Font_BRepFont_initNameAspectAndSize" rawInitFromNameAspectAndSize :: Ptr BRepFont ->  CString -> CInt -> CDouble -> IO CBool

initFromNameAspectAndSize :: Ptr BRepFont -> String -> FontAspect -> Double -> IO Bool
initFromNameAspectAndSize font fontname aspect size = 
    withCString fontname $ \str -> cBoolToBool <$> rawInitFromNameAspectAndSize font str (fromIntegral . fromEnum $ aspect) (coerce size)


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