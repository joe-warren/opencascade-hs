#include <XCAFDoc_DocumentTool.hxx>
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