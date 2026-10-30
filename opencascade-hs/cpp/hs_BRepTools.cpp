#include <BRepTools.hxx>
#include <TopoDS_Wire.hxx>
#include <TopoDS_Face.hxx>
#include "hs_Exception.h"
#include "hs_BRepTools.h"

TopoDS_Wire * hs_BRepTools_OuterWire(
        TopoDS_Face * face,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [face]{
            return new TopoDS_Wire(BRepTools::OuterWire(*face));
        }
    );
}