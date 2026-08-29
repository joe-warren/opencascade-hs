#include <XCAFDoc_DocumentTool.hxx>
#include <XCAFDoc_ColorTool.hxx>
#include <XCAFDoc_VisMaterialTool.hxx>
#include "hs_Exception.h"
#include "hs_XCAFDoc_DocumentTool.h"

Handle (XCAFDoc_ShapeTool) * hs_XCAFDoc_DocumentTool_shapeTool(
        TDF_Label * label,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [label]{
        return new opencascade::handle<XCAFDoc_ShapeTool>(XCAFDoc_DocumentTool::ShapeTool(*label));
    });
}
Handle (XCAFDoc_ColorTool) * hs_XCAFDoc_DocumentTool_colorTool(
        TDF_Label * label,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [label]{
        return new opencascade::handle<XCAFDoc_ColorTool>(XCAFDoc_DocumentTool::ColorTool(*label));
    });
}

Handle (XCAFDoc_VisMaterialTool) * hs_XCAFDoc_DocumentTool_visMaterialTool(
        TDF_Label * label,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [label]{
        return new opencascade::handle<XCAFDoc_VisMaterialTool>(XCAFDoc_DocumentTool::VisMaterialTool(*label));
    });
}
