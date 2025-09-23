module OpenCascade.BRepBuilderAPI.Sewing
( Sewing
, new
, load
, add
, perform
, sewedShape
, nbFreeEdges
, nbContigousEdges
, nbMultipleEdges
) where 

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.BRepBuilderAPI.Internal.Destructors (deleteSewing)
import OpenCascade.BRepBuilderAPI.Types (Sewing)
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CDouble (..), CInt (..))
import Data.Acquire (Acquire, mkAcquire)

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_Sewing.hxx>"
C.include "<TopoDS_Shape.hxx>"

new :: Double -> Bool -> Bool -> Bool -> Bool -> Acquire (Ptr Sewing)
new tolerance opt1 opt2 opt3 opt4 = mkAcquire createSewing deleteSewing
  where
    createSewing = 
      let cTolerance = realToFrac tolerance
          cOpt1 = boolToCBool opt1
          cOpt2 = boolToCBool opt2
          cOpt3 = boolToCBool opt3
          cOpt4 = boolToCBool opt4
      in [C.throwBlock| BRepBuilderAPI_Sewing* {
        return new BRepBuilderAPI_Sewing($(double cTolerance), $(bool cOpt1), $(bool cOpt2), $(bool cOpt3), $(bool cOpt4));
      } |]

load :: Ptr Sewing -> Ptr TopoDS.Shape -> IO ()
load sewing shape = [C.throwBlock| void {
  $(BRepBuilderAPI_Sewing* sewing)->Load(*$(TopoDS_Shape* shape));
} |]

add :: Ptr Sewing -> Ptr TopoDS.Shape -> IO ()
add sewing shape = [C.throwBlock| void {
  $(BRepBuilderAPI_Sewing* sewing)->Add(*$(TopoDS_Shape* shape));
} |]

perform :: Ptr Sewing -> IO ()
perform sewing = [C.throwBlock| void {
  $(BRepBuilderAPI_Sewing* sewing)->Perform();
} |]

sewedShape :: Ptr Sewing -> Acquire (Ptr TopoDS.Shape)
sewedShape sewing = mkAcquire createSewedShape deleteShape
  where
    createSewedShape = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(BRepBuilderAPI_Sewing* sewing)->SewedShape());
    } |]

nbFreeEdges :: Ptr Sewing -> IO Int
nbFreeEdges sewing = do
  result <- [C.throwBlock| int {
    return $(BRepBuilderAPI_Sewing* sewing)->NbFreeEdges();
  } |]
  return (fromIntegral result)

nbContigousEdges :: Ptr Sewing -> IO Int
nbContigousEdges sewing = do
  result <- [C.throwBlock| int {
    return $(BRepBuilderAPI_Sewing* sewing)->NbContigousEdges();
  } |]
  return (fromIntegral result)

nbMultipleEdges :: Ptr Sewing -> IO Int
nbMultipleEdges sewing = do
  result <- [C.throwBlock| int {
    return $(BRepBuilderAPI_Sewing* sewing)->NbMultipleEdges();
  } |]
  return (fromIntegral result)