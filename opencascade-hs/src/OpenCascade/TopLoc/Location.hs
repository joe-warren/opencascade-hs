module OpenCascade.TopLoc.Location
( Location
, new
, fromGPTrsf
, isIdentity
, firstPower
, nextLocation
, inverted
, multiplied
, divided
, predivided
, powered
, isEqual
, isDifferent
, clear
, toGPTrsf
) where

import OpenCascade.TopLoc.Types
import OpenCascade.TopLoc.Internal.Context
import OpenCascade.TopLoc.Internal.Destructors
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.GP.Internal.Destructors (deleteTrsf)
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire 
import qualified OpenCascade.GP as GP

C.context (C.cppCtx <> gpContext <> topLocContext)

C.include "<TopLoc_Location.hxx>"
C.include "<gp_Trsf.hxx>"

-- new 

new :: Acquire (Ptr Location)
new = mkAcquire createLocation deleteLocation
  where
    createLocation = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location();
    } |]

-- from GP.Trsf

fromGPTrsf :: Ptr (GP.Trsf) -> Acquire (Ptr Location)
fromGPTrsf trsf = mkAcquire createFromTrsf deleteLocation
  where
    createFromTrsf = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location(*$(gp_Trsf* trsf));
    } |]

-- isIdentity

isIdentity :: Ptr Location -> IO Bool
isIdentity location = do
  result <- [C.throwBlock| bool {
    return $(TopLoc_Location* location)->IsIdentity();
  } |]
  return (cBoolToBool result)

-- firstPower 

firstPower :: Ptr Location -> IO Int
firstPower location = do
  result <- [C.throwBlock| int {
    return $(TopLoc_Location* location)->FirstPower();
  } |]
  return (fromIntegral result) 

-- nextLocation

nextLocation :: Ptr Location -> Acquire (Ptr Location)
nextLocation location = mkAcquire createNext deleteLocation
  where
    createNext = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location($(TopLoc_Location* location)->NextLocation());
    } |]

-- inverted

inverted :: Ptr Location -> Acquire (Ptr Location)
inverted location = mkAcquire createInverted deleteLocation
  where
    createInverted = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location($(TopLoc_Location* location)->Inverted());
    } |]

-- multiplied

multiplied :: Ptr Location -> Ptr Location -> Acquire (Ptr Location)
multiplied locA locB = mkAcquire createMultiplied deleteLocation
  where
    createMultiplied = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location($(TopLoc_Location* locA)->Multiplied(*$(TopLoc_Location* locB)));
    } |]

-- divided

divided :: Ptr Location -> Ptr Location -> Acquire (Ptr Location)
divided locA locB = mkAcquire createDivided deleteLocation
  where
    createDivided = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location($(TopLoc_Location* locA)->Divided(*$(TopLoc_Location* locB)));
    } |]

-- predivided

predivided :: Ptr Location -> Ptr Location -> Acquire (Ptr Location)
predivided locA locB = mkAcquire createPredivided deleteLocation
  where
    createPredivided = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location($(TopLoc_Location* locA)->Predivided(*$(TopLoc_Location* locB)));
    } |]

-- powered

powered :: Ptr Location -> Int -> Acquire (Ptr Location)
powered location power = mkAcquire createPowered deleteLocation
  where
    createPowered = 
      let cPower = fromIntegral power
      in [C.throwBlock| TopLoc_Location* {
        return new TopLoc_Location($(TopLoc_Location* location)->Powered($(int cPower)));
      } |]

-- isEqual

isEqual :: Ptr Location -> Ptr Location -> IO Bool
isEqual locA locB = do
  result <- [C.throwBlock| bool {
    return $(TopLoc_Location* locA)->IsEqual(*$(TopLoc_Location* locB));
  } |]
  return (cBoolToBool result)

-- isDifferent

isDifferent :: Ptr Location -> Ptr Location -> IO Bool
isDifferent locA locB = do
  result <- [C.throwBlock| bool {
    return $(TopLoc_Location* locA)->IsDifferent(*$(TopLoc_Location* locB));
  } |]
  return (cBoolToBool result)

-- clear

clear :: Ptr Location -> IO ()
clear location = [C.throwBlock| void {
  $(TopLoc_Location* location)->Clear();
} |]

-- toGPTrsf

toGPTrsf :: Ptr Location -> Acquire (Ptr GP.Trsf)
toGPTrsf location = mkAcquire createTrsf deleteTrsf
  where
    createTrsf = [C.throwBlock| gp_Trsf* {
      return new gp_Trsf($(TopLoc_Location* location)->Transformation());
    } |]
