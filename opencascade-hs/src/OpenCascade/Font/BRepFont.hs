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

import OpenCascade.Font.Internal.Context
import OpenCascade.Font.Types (BRepFont)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C
import Foreign.Ptr
import OpenCascade.Font.Internal.Destructors (deleteBRepFont)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Font.FontAspect (FontAspect)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> fontContext)

C.include "<Font_BRepFont.hxx>"
C.include "<Font_FontAspect.hxx>"

new :: Acquire (Ptr BRepFont)
new =
  let createFont = [C.throwBlock| Font_BRepFont* {
        return new Font_BRepFont();
      } |]
  in mkAcquire createFont deleteBRepFont

fromPathAndSize :: FilePath -> Double -> Acquire (Ptr BRepFont)
fromPathAndSize path size =
  let cSize = realToFrac size
      createFont = withCString path $ \cPath -> [C.throwBlock| Font_BRepFont* {
        return new Font_BRepFont($(char* cPath), $(double cSize));
      } |]
  in mkAcquire createFont deleteBRepFont

initFromPathAndSize :: Ptr BRepFont -> FilePath -> Double -> IO Bool
initFromPathAndSize font fontPath size = do
  let cSize = realToFrac size
      cFaceId = 0 :: CInt  -- Default face ID
  result <- withCString fontPath $ \cPath -> [C.throwBlock| bool {
    return $(Font_BRepFont* font)->Init($(char* cPath), $(double cSize), $(int cFaceId));
  } |]
  return (cBoolToBool result)

initFromNameAspectAndSize :: Ptr BRepFont -> String -> FontAspect -> Double -> IO Bool
initFromNameAspectAndSize font fontname aspect size = do
  let cAspect = fromIntegral $ fromEnum aspect
      cSize = realToFrac size
  result <- withCString fontname $ \cName -> [C.throwBlock| bool {
    return $(Font_BRepFont* font)->Init(
      $(char* cName), 
      static_cast<Font_FontAspect>($(int cAspect)), 
      $(double cSize)
    );
  } |]
  return (cBoolToBool result)


ascender :: Ptr BRepFont -> IO Double
ascender font = do
  result <- [C.throwBlock| double {
    return $(Font_BRepFont* font)->Ascender();
  } |]
  return (realToFrac result)

descender :: Ptr BRepFont -> IO Double
descender font = do
  result <- [C.throwBlock| double {
    return $(Font_BRepFont* font)->Descender();
  } |]
  return (realToFrac result)

lineSpacing :: Ptr BRepFont -> IO Double
lineSpacing font = do
  result <- [C.throwBlock| double {
    return $(Font_BRepFont* font)->LineSpacing();
  } |]
  return (realToFrac result)

pointSize :: Ptr BRepFont -> IO Double
pointSize font = do
  result <- [C.throwBlock| double {
    return $(Font_BRepFont* font)->PointSize();
  } |]
  return (realToFrac result)