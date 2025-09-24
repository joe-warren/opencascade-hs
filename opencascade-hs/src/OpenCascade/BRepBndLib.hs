module OpenCascade.BRepBndLib 
( add
, addOptimal
, addOBB
) where 

import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.Bnd.Internal.Context (bndContext)
import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.Bnd.Types (Box, OBB)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> bndContext)

C.include "<BRepBndLib.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<Bnd_Box.hxx>"
C.include "<Bnd_OBB.hxx>"

add :: Ptr Shape -> Ptr Box -> Bool -> IO ()
add shape box useTriangulation = do
  let cUseTriangulation = boolToCBool useTriangulation
  [C.throwBlock| void {
    BRepBndLib::Add(*$(TopoDS_Shape* shape), *$(Bnd_Box* box), $(bool cUseTriangulation));
  } |]

addOptimal :: Ptr Shape -> Ptr Box -> Bool -- ^ useTriangulation
    -> Bool -- ^ useShapeTolerance
    -> IO ()
addOptimal shape box useTriangulation useShapeTolerance = do
  let cUseTriangulation = boolToCBool useTriangulation
      cUseShapeTolerance = boolToCBool useShapeTolerance
  [C.throwBlock| void {
    BRepBndLib::AddOptimal(*$(TopoDS_Shape* shape), *$(Bnd_Box* box), $(bool cUseTriangulation), $(bool cUseShapeTolerance));
  } |]

addOBB :: Ptr Shape -> Ptr OBB -> Bool -- ^ isTriangulation used
    -> Bool -- ^ isOptimal
    -> Bool -- ^ is ShapeToleranceUsed
    -> IO ()
addOBB shape obb triangulationUsed optimal shapeToleranceUsed = do
  let cTriangulationUsed = boolToCBool triangulationUsed
      cOptimal = boolToCBool optimal
      cShapeToleranceUsed = boolToCBool shapeToleranceUsed
  [C.throwBlock| void {
    BRepBndLib::AddOBB(*$(TopoDS_Shape* shape), *$(Bnd_OBB* obb), $(bool cTriangulationUsed), $(bool cOptimal), $(bool cShapeToleranceUsed));
  } |]