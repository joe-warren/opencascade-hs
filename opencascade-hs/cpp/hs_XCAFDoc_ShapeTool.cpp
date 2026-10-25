#include <XCAFDoc_ShapeTool.hxx>
#include <TDF_Label.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_XCAFDoc_ShapeTool.h"

void hs_delete_XCAFDoc_ShapeTool(Handle(XCAFDoc_ShapeTool) * shapeTool){
    delete shapeTool;
}

TDF_Label * hs_XCAFDoc_ShapeTool_addShape(
        Handle(XCAFDoc_ShapeTool) *shapeTool, TopoDS_Shape *theShape, bool makeAssembly, bool makePrepare,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [shapeTool, theShape, makeAssembly, makePrepare]{
        return new TDF_Label(shapeTool->get()->AddShape(*theShape, makeAssembly, makePrepare));
    });
}
TDF_Label * hs_XCAFDoc_ShapeTool_addSubShape(
        Handle(XCAFDoc_ShapeTool) *shapeTool, TDF_Label *shapeLabel, TopoDS_Shape *subShape,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [shapeTool, shapeLabel, subShape]{
        return new TDF_Label(shapeTool->get()->AddSubShape(*shapeLabel, *subShape));
    });
}

TDF_Label * hs_XCAFDoc_ShapeTool_findShape(
        Handle(XCAFDoc_ShapeTool) *shapeTool, TopoDS_Shape *theShape, bool findInstance,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [shapeTool, theShape, findInstance]{
        return new TDF_Label(shapeTool->get()->FindShape(*theShape, findInstance));
    });
}
