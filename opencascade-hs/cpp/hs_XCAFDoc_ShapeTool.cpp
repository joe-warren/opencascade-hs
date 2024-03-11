#include <XCAFDoc_ShapeTool.hxx>
#include "hs_XCAFDoc_ShapeTool.h"

void hs_delete_XCAFDoc_ShapeTool(Handle(XCAFDoc_ShapeTool) * shapeTool){
    delete shapeTool;
}

TDF_Label * hs_XCAFDoc_ShapeTool_addShape(Handle(XCAFDoc_ShapeTool) *shapeTool, TopoDS_Shape *theShape, bool makeAssembly, bool makePrepare){
    return new TDF_Label(shapeTool->get()->AddShape(*theShape, makeAssembly, makePrepare));
}