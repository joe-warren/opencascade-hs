module OpenCascade.Font.BRepTextBuilder
( BRepTextBuilder
, new
, perform
) where

import OpenCascade.Font.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.Font.Types
import OpenCascade.Font.Internal.Destructors
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import qualified OpenCascade.Graphic3D.VerticalTextAlignment as VTA
import qualified OpenCascade.Graphic3D.HorizontalTextAlignment as HTA
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> gpContext <> topoDSContext <> fontContext)

C.include "<Font_BRepTextBuilder.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<gp_Ax3.hxx>"
C.include "<Graphic3d_VerticalTextAlignment.hxx>"
C.include "<Graphic3d_HorizontalTextAlignment.hxx>"

-- Needed to avoid a linker error about duplicate symbols with BRepFont
C.verbatim "extern template class NCollection_UtfIterator<char>;"

new :: Acquire (Ptr BRepTextBuilder)
new =
  let createBuilder = [C.throwBlock| Font_BRepTextBuilder* {
        return new Font_BRepTextBuilder();
      } |]
  in mkAcquire createBuilder deleteBRepTextBuilder

perform :: Ptr BRepTextBuilder -> Ptr BRepFont -> String -> Ptr GP.Ax3 -> HTA.HorizontalTextAlignment -> VTA.VerticalTextAlignment -> Acquire (Ptr TopoDS.Shape)
perform builder font str axis hAlign vAlign =
  let cHAlign = fromIntegral $ fromEnum hAlign
      cVAlign = fromIntegral $ fromEnum vAlign
      createShape = withCString str $ \cStr -> [C.throwBlock| TopoDS_Shape* {
        return new TopoDS_Shape($(Font_BRepTextBuilder* builder)->Perform(
          *$(Font_BRepFont* font),
          $(char* cStr),
          *$(gp_Ax3* axis),
          static_cast<Graphic3d_HorizontalTextAlignment>($(int cHAlign)),
          static_cast<Graphic3d_VerticalTextAlignment>($(int cVAlign))
        ));
      } |]
  in mkAcquire createShape deleteShape
