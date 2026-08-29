#include <XCAFDoc_VisMaterialTool.hxx>
#include <XCAFDoc_VisMaterial.hxx>
#include <TDF_Label.hxx>
#include <TopoDS_Shape.hxx>
#include <TCollection_AsciiString.hxx>
#include "hs_Exception.h"
#include "hs_XCAFDoc_VisMaterialTool.h"

void hs_delete_XCAFDoc_VisMaterialTool(Handle(XCAFDoc_VisMaterialTool) * materialTool){
    delete materialTool;
}

TDF_Label * hs_XCAFDoc_VisMaterialTool_addMaterial(
        Handle(XCAFDoc_VisMaterialTool) * materialTool, Handle(XCAFDoc_VisMaterial) * material, char * name,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [materialTool, material, name]{
        return new TDF_Label((*materialTool)->AddMaterial(*material, TCollection_AsciiString(name)));
    });
}

void hs_XCAFDoc_VisMaterialTool_setShapeMaterial(
        Handle(XCAFDoc_VisMaterialTool) * materialTool, TDF_Label * shapeLabel, TDF_Label * materialLabel,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(exType, exPtr, [materialTool, shapeLabel, materialLabel]{
        (*materialTool)->SetShapeMaterial(*shapeLabel, *materialLabel);
    });
}

bool hs_XCAFDoc_VisMaterialTool_setShapeMaterialFromShape(
        Handle(XCAFDoc_VisMaterialTool) * materialTool, TopoDS_Shape * shape, TDF_Label * materialLabel,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(exType, exPtr, [materialTool, shape, materialLabel]{
        return (*materialTool)->SetShapeMaterial(*shape, *materialLabel) == Standard_True;
    }, false);
}
