module OpenCascade.BRepOffsetAPI.ThruSections
( ThruSections
, new
, addWire
, addVertex
) where

import OpenCascade.BRepOffsetAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepOffsetAPI.Types (ThruSections)
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteThruSections)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Foreign.C (CBool (..), CDouble (..))
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> brepOffsetAPIContext)

C.include "<BRepOffsetAPI_ThruSections.hxx>"
C.include "<TopoDS_Wire.hxx>"
C.include "<TopoDS_Vertex.hxx>"


new :: Bool -> Bool -> Double -> Acquire (Ptr ThruSections)
new makeSolid ruled precision =
  let cMakeSolid = boolToCBool makeSolid
      cRuled = boolToCBool ruled
      cPrecision = realToFrac precision
      createThruSections = [C.throwBlock| BRepOffsetAPI_ThruSections* {
        return new BRepOffsetAPI_ThruSections($(bool cMakeSolid), $(bool cRuled), $(double cPrecision));
      } |]
  in mkAcquire createThruSections deleteThruSections

addWire :: Ptr ThruSections -> Ptr TopoDS.Wire -> IO ()
addWire sections wire = [C.throwBlock| void {
  $(BRepOffsetAPI_ThruSections* sections)->AddWire(*$(TopoDS_Wire* wire));
} |]

addVertex :: Ptr ThruSections -> Ptr TopoDS.Vertex -> IO ()
addVertex sections vertex = [C.throwBlock| void {
  $(BRepOffsetAPI_ThruSections* sections)->AddVertex(*$(TopoDS_Vertex* vertex));
} |]