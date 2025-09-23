module OpenCascade.GP.XYZ 
( XYZ
, newXYZ
, fromDoubles
, setX, setY, setZ
, x, y, z
) where

import OpenCascade.GP.Types (XYZ)
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors (deleteXYZ)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

C.context (C.cppCtx <> gpContext)

C.include "<gp_XYZ.hxx>"

newXYZ :: Acquire (Ptr XYZ)
newXYZ = mkAcquire createXYZ deleteXYZ
  where
    createXYZ = [C.throwBlock| gp_XYZ* {
      return new gp_XYZ();
    } |]

fromDoubles :: Double -> Double -> Double -> Acquire (Ptr XYZ)
fromDoubles x' y' z' = mkAcquire createXYZ deleteXYZ
  where
    createXYZ = let
      cx = realToFrac x'
      cy = realToFrac y'
      cz = realToFrac z'
      in [C.throwBlock| gp_XYZ* {
        return new gp_XYZ($(double cx), $(double cy), $(double cz));
      } |]

setX :: Ptr XYZ -> Double -> IO ()
setX xyz val = 
  let cVal = realToFrac val
  in [C.throwBlock| void {
    $(gp_XYZ* xyz)->SetX($(double cVal));
  } |]

setY :: Ptr XYZ -> Double -> IO ()
setY xyz val = 
  let cVal = realToFrac val
  in [C.throwBlock| void {
    $(gp_XYZ* xyz)->SetY($(double cVal));
  } |]

setZ :: Ptr XYZ -> Double -> IO ()
setZ xyz val = 
  let cVal = realToFrac val
  in [C.throwBlock| void {
    $(gp_XYZ* xyz)->SetZ($(double cVal));
  } |]

x :: Ptr XYZ -> IO Double
x xyz = do
  result <- [C.throwBlock| double {
    return $(gp_XYZ* xyz)->X();
  } |]
  return (realToFrac result)

y :: Ptr XYZ -> IO Double
y xyz = do
  result <- [C.throwBlock| double {
    return $(gp_XYZ* xyz)->Y();
  } |]
  return (realToFrac result)

z :: Ptr XYZ -> IO Double
z xyz = do
  result <- [C.throwBlock| double {
    return $(gp_XYZ* xyz)->Z();
  } |]
  return (realToFrac result)