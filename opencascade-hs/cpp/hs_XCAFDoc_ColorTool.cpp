#include <XCAFDoc_ColorTool.hxx>
#include <TDF_Label.hxx>
#include <Quantity_Color.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_XCAFDoc_ColorTool.h"

void hs_delete_XCAFDoc_ColorTool(Handle(XCAFDoc_ColorTool) * colorTool){
    delete colorTool;
}

void hs_XCAFDoc_ColorTool_setColor(
        Handle(XCAFDoc_ColorTool) * colorTool, TDF_Label * label, Quantity_Color * color, XCAFDoc_ColorType colorType,
        HSExceptionType* exType, void ** exPtr
    ){
    hs_handleExVoid(exType, exPtr, [colorTool, label, color, colorType]{
        (*colorTool)->SetColor(*label, *color, colorType);
    });
}

bool hs_XCAFDoc_ColorTool_setShapeColor(
        Handle(XCAFDoc_ColorTool) * colorTool, TopoDS_Shape * shape, Quantity_Color * color, XCAFDoc_ColorType colorType,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleExWithDefault(exType, exPtr, [colorTool, shape, color, colorType]{
        return (*colorTool)->SetColor(*shape, *color, colorType);
    }, false);
}
